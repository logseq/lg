(ns ^:no-doc datascript.db
  (:require
   [clojure.walk]
   [clojure.data]
   [datascript.schema :as ds]
   [datascript.util :as util]
   [ocaml.package/datascript.runtime]
   [me.tonsky.persistent-sorted-set :as set]
   [me.tonsky.persistent-sorted-set.arrays :as arrays])
  #?(:cljs (:require-macros [datascript.db :refer [case-tree combine-cmp declare+ defn+ defcomp int-compare validate-attr validate-val]])))

#?(:clj (set! *warn-on-reflection* true))

;; ----------------------------------------------------------------------------

(def ^:const e0
  0)

(def ^:const tx0
  0x20000000)

(def ^:const emax
  0x7FFFFFFF)

(def ^:const txmax
  0x7FFFFFFF)

(def  empty-schema-entry
  {})

(def  empty-schema
  {})

(signature datascript.db/effective-schema
  :fn<option<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>>;map<keyword;map<keyword;Datascript_runtime.Data_value.t>>>)
(defn effective-schema
  [maybe-schema]
  (if-some [schema maybe-schema] schema empty-schema))

(defn schema-map
  {:inline
   (fn [source]
     (let [data-value
           (fn data-value [value]
             (cond
               (nil? value)
               (list 'Datascript_runtime.Data_value.Nil)

               (string? value)
               (list 'Datascript_runtime.Data_value.String value)

               (keyword? value)
               (list
                'Datascript_runtime.Data_value.Keyword
                (str value))

               (= value true)
               (list 'Datascript_runtime.Data_value.Bool true)

               (= value false)
               (list 'Datascript_runtime.Data_value.Bool false)

               (float? value)
               (list 'Datascript_runtime.Data_value.Float value)

               (vector? value)
               (list
                'Datascript_runtime.Data_value.vector_of_vector
                (list
                 'vec
                 (cons
                  'list
                  (map data-value value))))

               (or (symbol? value) (seq? value))
               value

               :else
               (list 'Datascript_runtime.Data_value.Int value)))]
       (if (map? source)
         (if (empty? source)
           'datascript.db/empty-schema
           (list
            'zipmap
            (vec (map first source))
            (vec
             (map
              (fn [entry]
                (let [properties (second entry)]
                  (list
                   'zipmap
                   (vec (map first properties))
                   (vec
                    (map
                     (fn [property]
                       (data-value (second property)))
                     properties)))))
              source))))
         source)))}
  [ source]
  source)

(def  empty-schema-idents
  {})

(def  empty-schema-drafts
  {})

(defn  implicit-schema-entry []
  (assoc
   empty-schema-entry
   :db/unique
   (Datascript_runtime.Data_value.Keyword ":db.unique/identity")))

(def implicit-schema
  (assoc empty-schema
         :db/ident
         (implicit-schema-entry)))

(defn  merge-schema
  [ base
    overrides]
  (reduce-kv
   (fn [result attr entry]
     (assoc result attr entry))
   base
   overrides))

;; ----------------------------------------------------------------------------

#?(:clj
   (defmacro declare+ [name & _arglists]
     `(declare ~name)))

#?(:clj
   (defmacro defn+ [name & body]
     `(defn ~name ~@body)))

#?(:clj
   (defmacro defcomp [name args & body]
     `(defn ~name ~args ~@body)))

(defn combine-hashes [left right]
  (hash-combine left right))

;; ----------------------------------------------------------------------------

(declare equiv-datom datom-hash)

(defprotocol IDatom
  (datom-tx [datom] :int)
  (datom-added [datom] :bool)
  (datom-get-idx [datom] :int)
  (datom-set-idx [datom value] :unit))

(deftype Datom [^long e
                ^:keyword a
                ^:Datascript_runtime.Data_value.t v
                ^long tx
                ^:mutable ^int idx
                ^:mutable ^int cached-hash]
  IDatom
  (datom-tx [_]
    (if (pos? tx) tx (- tx)))
  (datom-added [_]
    (pos? tx))
  (datom-get-idx [_] idx)
  (datom-set-idx [_ value]
    (set! idx value)
    (Stdlib.ignore 0))

  IEquiv
  (-equiv [d o]
    (equiv-datom d o))

  IHash
  (-hash [d]
    (if (zero? cached-hash)
      (let [value (datom-hash d)]
        (set! cached-hash value)
        value)
      cached-hash)))

(defn datom-attr [datom]
  (.-a datom))

(defn datom-print-string [datom]
  (str
   "#datascript/Datom ["
   (.-e datom)
   " "
   (.-a datom)
   " "
   (Datascript_runtime.Data_value.to_edn_string (.-v datom))
   " "
   (datom-tx datom)
   " "
   (datom-added datom)
   "]"))

(defmethod print-method Datom
  [ datom  writer]
  (Buffer.add_string writer (datom-print-string datom)))

(defn  datom-closed
  ([ e
     a
     v]
   (Datom. e a v tx0 0 0))
  ([ e
     a
     v
     tx]
   (Datom. e a v tx 0 0))
  ([ e
     a
     v
     tx
    added]
   (Datom. e a v (if added tx (- tx)) 0 0)))

(defn datom
  {:inline
   (fn [e a v & rest]
     (let [value
           (if (string? v)
             (list
              'Datascript_runtime.Data_value.String
              v)
             (if (keyword? v)
               (list
                'Datascript_runtime.Data_value.Keyword
                (str v))
               (if (= v true)
                 (list
                  'Datascript_runtime.Data_value.Bool
                  true)
                 (if (= v false)
                   (list
                    'Datascript_runtime.Data_value.Bool
                    false)
                   (if (or (symbol? v) (seq? v))
                     v
                     (list
                      'Datascript_runtime.Data_value.Int
                      v))))))]
       (cons
        'datascript.db/datom-closed
        (cons e (cons a (cons value rest))))))}
  ([ e
     a
     v]
   (datom-closed e a v))
  ([ e
     a
     v
     tx]
   (datom-closed e a v tx))
  ([ e
     a
     v
     tx
    added]
   (datom-closed e a v tx added)))

(defn datom? [value]
  (satisfies? IDatom value))

(defn  datom-from-reader
  [ value]
  (datom-closed
   (Datascript_runtime.Serialization_value.reader_datom_entity value)
   (keyword
    (Datascript_runtime.Serialization_value.reader_datom_attribute value))
   (Datascript_runtime.Serialization_value.reader_datom_value value)
   (Datascript_runtime.Serialization_value.reader_datom_transaction value)
   (Datascript_runtime.Serialization_value.reader_datom_added value)))

(defn  datom-from-edn-string [ source]
  (datom-from-reader
   (Datascript_runtime.Serialization_value.read_datom source)))

(def ^:private attr-wildcard (keyword ""))
(def ^:private value-wildcard (Datascript_runtime.Data_value.Nil))

(defn  datom-bound
  [ e
    a
    v
    tx
    default-e
    default-tx]
  (datom
   (if-some [value e] value default-e)
   (if-some [value a] value attr-wildcard)
   (if-some [value v] value value-wildcard)
   (if-some [value tx] value default-tx)))

(defn ^:private equiv-datom [d o]
  (and (== (.-e d) (.-e o))
       (= (.-a d) (.-a o))
       (Datascript_runtime.Data_value.equal (.-v d) (.-v o))))

(signature datascript.db/datom-vectors-equal?
  :fn<vector<datascript.db/Datom>;vector<datascript.db/Datom>;bool>)

(defn datom-vectors-equal?
  [ left  right]
  (let [length (count left)]
    (and
     (= length (count right))
     (loop [index 0]
       (if (< index length)
         (if (equiv-datom (nth left index) (nth right index))
           (recur (inc index))
           false)
         true)))))

;; ----------------------------------------------------------------------------
;; datom cmp macros/funcs
;;

#?(:clj
   (defmacro combine-cmp [& comps]
     (loop [comps (reverse comps)
            res   (num 0)]
       (if (not-empty comps)
         (recur
          (next comps)
          `(let [c# ~(first comps)]
             (if (== 0 c#)
               ~res
               c#)))
         res))))

#?(:clj
   (defn- -case-tree [queries variants]
     (if queries
       (let [v1 (take (/ (count variants) 2) variants)
             v2 (drop (/ (count variants) 2) variants)]
         (list 'if (first queries)
               (-case-tree (next queries) v1)
               (-case-tree (next queries) v2)))
       (first variants))))

#?(:clj
   (defmacro case-tree [qs vs]
     (-case-tree qs vs)))

(defn cmp
   [ x  y]
  (if (= x attr-wildcard)
    0
    (if (= y attr-wildcard) 0 (long (compare x y)))))

(def ^:private int-compare-less -1)
(def ^:private int-compare-equal 0)
(def ^:private int-compare-greater 1)

(defmacro int-compare [x y]
  `(let [left# ~x
         right# ~y]
     (cond
       (< left# right#) int-compare-less
       (> left# right#) int-compare-greater
       :else int-compare-equal)))

(defn-  data-value-class-name
  [ value]
  (Datascript_runtime.Data_value.class_name value))

(defn  class-identical?
  [ left
    right]
  (= (data-value-class-name left) (data-value-class-name right)))

(defn  class-compare
  [ left
    right]
  (compare (data-value-class-name left) (data-value-class-name right)))

(defn  ihash [ value]
  (Datascript_runtime.Data_value.hash value))

(defn  seqable? [ value]
  (match value
    Datascript_runtime.Data_value.Nil true
    (Datascript_runtime.Data_value.List _) true
    (Datascript_runtime.Data_value.Vector _) true
    (Datascript_runtime.Data_value.Map _) true
    (Datascript_runtime.Data_value.Set _) true
    (Datascript_runtime.Data_value.Tuple _) true
    _ false))

(defn  value-compare
  [ x
    y]
  (Datascript_runtime.Data_value.compare x y))

(defn value-cmp
   [ x
          y]
  (if (Datascript_runtime.Data_value.is_nil x)
    0
    (if (Datascript_runtime.Data_value.is_nil y)
      0
      (value-compare x y))))

;; Slower cmp-* fns allows for datom fields to be nil.
;; Such datoms come from slice method where they are used as boundary markers.

(defn cmp-datoms-eavt [d1 d2]
  (combine-cmp
   (int-compare (.-e d1) (.-e d2))
   (cmp (.-a d1) (.-a d2))
   (value-cmp (.-v d1) (.-v d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

(defn cmp-datoms-aevt [d1 d2]
  (combine-cmp
   (cmp (.-a d1) (.-a d2))
   (int-compare (.-e d1) (.-e d2))
   (value-cmp (.-v d1) (.-v d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

(defn cmp-datoms-avet [d1 d2]
  (combine-cmp
   (cmp (.-a d1) (.-a d2))
   (value-cmp (.-v d1) (.-v d2))
   (int-compare (.-e d1) (.-e d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

;; fast versions without nil checks

(defn- cmp-attr-quick  [ a1  a2]
  (compare a1 a2))

(defn cmp-datoms-eav-quick [d1 d2]
  (combine-cmp
   (int-compare (.-e d1) (.-e d2))
   (cmp-attr-quick (.-a d1) (.-a d2))
   (value-compare (.-v d1) (.-v d2))))

(defn cmp-datoms-eavt-quick [d1 d2]
  (combine-cmp
   (int-compare (.-e d1) (.-e d2))
   (cmp-attr-quick (.-a d1) (.-a d2))
   (value-compare (.-v d1) (.-v d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

(defn cmp-datoms-aevt-quick [d1 d2]
  (combine-cmp
   (cmp-attr-quick (.-a d1) (.-a d2))
   (int-compare (.-e d1) (.-e d2))
   (value-compare (.-v d1) (.-v d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

(defn cmp-datoms-avet-quick [d1 d2]
  (combine-cmp
   (cmp-attr-quick (.-a d1) (.-a d2))
   (value-compare (.-v d1) (.-v d2))
   (int-compare (.-e d1) (.-e d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

;; ----------------------------------------------------------------------------

(signature datascript.db/typed-index
  :fn<datascript.db/DB;keyword;set/btset<datascript.db/Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>>>)

(declare restore-db typed-index)

(signature datascript.db/components->pattern
  :fn<datascript.db/DB;keyword;option<Datascript_runtime.Data_value.t>;option<Datascript_runtime.Data_value.t>;option<Datascript_runtime.Data_value.t>;option<Datascript_runtime.Data_value.t>;int;int;datascript.db/Datom>)
(signature datascript.db/resolve-datom
  :fn<datascript.db/DB;option<Datascript_runtime.Data_value.entity_ref>;option<keyword>;option<Datascript_runtime.Data_value.t>;option<Datascript_runtime.Data_value.entity_ref>;int;int;datascript.db/Datom>)

(declare resolve-datom components->pattern)

;;;;;;;;;; Fast validation

#?(:clj
   (defmacro validate-attr [attr at]
     `(let [attr# ~attr]
        (when-not (or
                   (keyword? attr#)
                   (string? attr#))
          (let [at# ~at]
            (util/raise "Bad entity attribute " attr# " at " at# ", expected keyword or string"
                        {:error :transact/syntax, :attribute attr#, :context at#}))))))

#?(:clj
   (defmacro validate-val [v at]
     `(when (Datascript_runtime.Data_value.is_nil ~v)
        (let [at# ~at]
          (util/raise "Cannot store nil as a value at " at#
                      {:error :transact/syntax, :value nil, :context at#})))))

;;;;;;;;;; Searching

(defprotocol ISearch
  (-search
   [data pattern]
   :seq<Datom>))

(defprotocol IIndexAccess
  (-datoms
   [db
    index
    c0
    c1
    c2
    c3]
   :seq<Datom>)
  (-seek-datoms
   [db
    index
    c0
    c1
    c2
    c3]
   :seq<Datom>)
  (-rseek-datoms
   [db
    index
    c0
    c1
    c2
    c3]
   :seq<Datom>)
  (-index-range
   [db
    ^:keyword attr
    ^:option<Datascript_runtime.Data_value.t> start
    ^:option<Datascript_runtime.Data_value.t> end]
   :seq<Datom>))

(defn  index-component-keyword
  [ component]
  (if-some [component component]
    (if-some [keyword-text
              (Datascript_runtime.Data_value.keyword_value component)]
      (Some (keyword keyword-text))
      (raise (Invalid_argument "Index attribute component must be a keyword")))
    None))

(defn  index-component-entity-ref
  [ component]
  (if-some [component component]
    (if-some
     [entity-ref
      (Datascript_runtime.Data_value.entity_ref_value component)]
      (Some entity-ref)
      (raise
       (Invalid_argument
        "Index entity component must be an entity reference")))
    None))

(defrecord ReverseSchema
  [^:set<keyword> unique-attrs
   ^:set<keyword> unique-identity-attrs
   ^:set<keyword> unique-value-attrs
   ^:set<keyword> indexed-attrs
   ^:set<keyword> many-attrs
   ^:set<keyword> ref-attrs
   ^:set<keyword> component-attrs
   ^:set<keyword> tuple-attrs
   ^:map<keyword;map<keyword;int>> attr-tuples])

(defprotocol IDB
  (-schema
   [db]
   :option<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>>)
  (-attrs-by [db property] :set<keyword>)
  (-attr-tuples [db] :map<keyword;map<keyword;int>>)
  (-unfiltered-db [db] :datascript.db/DB))

;; ----------------------------------------------------------------------------

(type-record tx-entity-form
  (values :map<keyword;Datascript_runtime.Data_value.t>)
  (attrs :vector<keyword>))

(type-variant tx-entry-of [db]
  TxNil
  (TxEntity :tx-entity-form)
  (TxCall :fn<db;vector<tx-entry-of<db>>>)
  (TxInstallFunction
   :map<keyword;Datascript_runtime.Data_value.t>
   :fn<db;Datascript_runtime.Data_value.t;vector<tx-entry-of<db>>>)
  (TxInvokeFunction
   :keyword
   :vector<Datascript_runtime.Data_value.t>)
  (TxInvalid :string)
  (TxAdd :Datascript_runtime.Data_value.entity_ref
         :keyword
         :Datascript_runtime.Data_value.t
         :option<int>)
  (TxRetract :Datascript_runtime.Data_value.entity_ref
             :keyword
             :option<Datascript_runtime.Data_value.t>)
  (TxCas :keyword
         :Datascript_runtime.Data_value.entity_ref
         :keyword
         :option<Datascript_runtime.Data_value.t>
         :Datascript_runtime.Data_value.t)
  (TxRetractAttribute :Datascript_runtime.Data_value.entity_ref :keyword)
  (TxRetractEntity :Datascript_runtime.Data_value.entity_ref)
  (TxSetTuple :Datascript_runtime.Data_value.entity_ref
              :keyword
              :option<Datascript_runtime.Data_value.t>)
  TxFlushTuples)

(def ^:private next-db-identity (atom 0))

(defn-  fresh-db-identity []
  (swap! next-db-identity
         (fn [current]
           (inc current))))

(defrecord DB [^:option<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>> schema
               ^:map<int;keyword> schema-idents
               ^:map<int;map<keyword;Datascript_runtime.Data_value.t>> schema-drafts
               ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> eavt
               ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> aevt
               ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> avet
               ^:int max-eid
               ^:int max-tx
               ^datascript.db/ReverseSchema rschema
               ^:map<keyword;fn<datascript.db/DB;Datascript_runtime.Data_value.t;vector<tx-entry-of<datascript.db/DB>>>> tx-functions
               ^:int identity-hash
               ^:ref<int> hash
               ^:map<keyword;Datascript_runtime.Data_value.t> metadata]
  IDB
  (-schema [db] (.-schema db))
  (-attrs-by [db property]
             (let [schema (.-rschema db)]
               (case property
                 :db/unique           (:unique-attrs schema)
                 :db.unique/identity  (:unique-identity-attrs schema)
                 :db.unique/value     (:unique-value-attrs schema)
                 :db/index            (:indexed-attrs schema)
                 :db.cardinality/many (:many-attrs schema)
                 :db.type/ref         (:ref-attrs schema)
                 :db/isComponent      (:component-attrs schema)
                 :db.type/tuple       (:tuple-attrs schema)
                 #{})))
  (-attr-tuples [db] (:attr-tuples (.-rschema db)))
  (-unfiltered-db [db] db)

  IEmptyableCollection
  (-empty [db]
    (assoc
     db
     :schema-idents empty-schema-idents
     :schema-drafts empty-schema-drafts
     :eavt (empty (.-eavt db))
     :aevt (empty (.-aevt db))
     :avet (empty (.-avet db))
     :max-eid e0
     :max-tx tx0
     :tx-functions {}
     :identity-hash (fresh-db-identity)
     :hash (atom 0)))

  ISearch
  (-search [db ^Datom pattern]
           (let [e          (if (= e0 (.-e pattern))
                              None
                              (Some (.-e pattern)))
                 a          (if (= attr-wildcard (.-a pattern))
                              None
                              (Some (.-a pattern)))
                 v          (if
                              (Datascript_runtime.Data_value.is_nil
                               (.-v pattern))
                              None
                              (Some (.-v pattern)))
                 tx         (if (= tx0 (datom-tx pattern))
                              None
                              (Some (datom-tx pattern)))
                 eavt       (.-eavt db)
                 aevt       (.-aevt db)
                 avet       (.-avet db)
                 pred       (fn [candidate]
                              (if-some [value v]
                                (= value candidate)
                                false))]
             (case-tree [e a (some? v) tx]
                        [(set/slice eavt (datom-bound e a v tx e0 tx0) (datom-bound e a v tx e0 tx0)) ;; e a v tx
                         (set/slice eavt (datom-bound e a v nil e0 tx0) (datom-bound e a v nil e0 txmax)) ;; e a v _
                         (->> (set/slice eavt (datom-bound e a nil nil e0 tx0) (datom-bound e a nil nil e0 txmax)) ;; e a _ tx
                              (->Eduction (filter (fn [d] (= tx (datom-tx d))))))
                         (set/slice eavt (datom-bound e a nil nil e0 tx0) (datom-bound e a nil nil e0 txmax)) ;; e a _ _
                         (->> (set/slice eavt (datom-bound e nil nil nil e0 tx0) (datom-bound e nil nil nil e0 txmax)) ;; e _ v tx
                              (->Eduction (filter (fn [d] (and (pred (.-v d))
                                                                      (= tx (datom-tx d)))))))
                         (->> (set/slice eavt (datom-bound e nil nil nil e0 tx0) (datom-bound e nil nil nil e0 txmax)) ;; e _ v _
                              (->Eduction (filter (fn [d] (pred (.-v d))))))
                         (->> (set/slice eavt (datom-bound e nil nil nil e0 tx0) (datom-bound e nil nil nil e0 txmax)) ;; e _ _ tx
                              (->Eduction (filter (fn [d] (= tx (datom-tx d))))))
                         (set/slice eavt (datom-bound e nil nil nil e0 tx0) (datom-bound e nil nil nil e0 txmax)) ;; e _ _ _
                         (if (if-some [attr a] (contains? (-attrs-by db :db/index) attr) false) ;; _ a v tx
                           (->> (set/slice avet (datom-bound nil a v nil e0 tx0) (datom-bound nil a v nil emax txmax))
                                (->Eduction (filter (fn [d] (= tx (datom-tx d))))))
                           (->> (set/slice aevt (datom-bound nil a nil nil e0 tx0) (datom-bound nil a nil nil emax txmax))
                                (->Eduction (filter (fn [d] (and (pred (.-v d))
                                                                        (= tx (datom-tx d))))))))
                         (if (if-some [attr a] (contains? (-attrs-by db :db/index) attr) false) ;; _ a v _
                           (set/slice avet (datom-bound nil a v nil e0 tx0) (datom-bound nil a v nil emax txmax))
                           (->> (set/slice aevt (datom-bound nil a nil nil e0 tx0) (datom-bound nil a nil nil emax txmax))
                                (->Eduction (filter (fn [d] (pred (.-v d)))))))
                         (->> (set/slice aevt (datom-bound nil a nil nil e0 tx0) (datom-bound nil a nil nil emax txmax)) ;; _ a _ tx
                              (->Eduction (filter (fn [d] (= tx (datom-tx d))))))
                         (set/slice aevt (datom-bound nil a nil nil e0 tx0) (datom-bound nil a nil nil emax txmax)) ;; _ a _ _
                         (filter (fn [d] (and (pred (.-v d))
                                                     (= tx (datom-tx d)))) (set/set-seq eavt))  ;; _ _ v tx
                         (filter (fn [d] (pred (.-v d))) (set/set-seq eavt))             ;; _ _ v
                         (filter (fn [d] (= tx (datom-tx d))) (set/set-seq eavt))        ;; _ _ _ tx
                         (set/set-seq eavt)])))                                                 ;; _ _ _ _

  IIndexAccess
  (-datoms [db index c0 c1 c2 c3]
           (when (= index :avet)
             (when-some [attr (index-component-keyword c0)]
               (when-not (contains? (-attrs-by db :db/index) attr)
                 (util/raise "Attribute " attr " should be marked as :db/index true"
                             {:error :index-access
                              :index :avet
                              :components (tuple c0 c1 c2 c3)}))))
           (set/slice (typed-index db index)
                      (components->pattern db index c0 c1 c2 c3 e0 tx0)
                      (components->pattern db index c0 c1 c2 c3 emax txmax)))

  (-seek-datoms [db index c0 c1 c2 c3]
                (when (= index :avet)
                  (when-some [attr (index-component-keyword c0)]
                    (when-not (contains? (-attrs-by db :db/index) attr)
                      (util/raise "Attribute " attr " should be marked as :db/index true"
                                  {:error :index-access
                                   :index :avet
                                   :components (tuple c0 c1 c2 c3)}))))
                (set/slice (typed-index db index)
                           (components->pattern db index c0 c1 c2 c3 e0 tx0)
                           (datom-bound nil nil nil nil emax txmax)))

  (-rseek-datoms [db index c0 c1 c2 c3]
                 (when (= index :avet)
                   (when-some [attr (index-component-keyword c0)]
                     (when-not (contains? (-attrs-by db :db/index) attr)
                       (util/raise "Attribute " attr " should be marked as :db/index true"
                                   {:error :index-access
                                    :index :avet
                                    :components (tuple c0 c1 c2 c3)}))))
                 (set/rslice (typed-index db index)
                             (components->pattern db index c0 c1 c2 c3 emax txmax)
                             (datom-bound nil nil nil nil e0 tx0)))

  (-index-range [db attr start end]
                (when-not (contains? (-attrs-by db :db/index) attr)
                  (util/raise "Attribute " attr " should be marked as :db/index true"
                              {:error :index-access
                               :index :avet
                               :components (tuple attr nil nil nil)}))
                (validate-attr attr (tuple '-index-range 'db attr start end))
                (set/slice (.-avet db)
                           (resolve-datom db nil attr start nil e0 tx0)
                           (resolve-datom db nil attr end nil emax txmax))))

(type-alias tx-entry :tx-entry-of<datascript.db/DB>)

(signature datascript.db/tx-entity
  :fn<map<keyword;Datascript_runtime.Data_value.t>;datascript.db/tx-entry>)

(signature datascript.db/tx-entity-ordered
  :fn<map<keyword;Datascript_runtime.Data_value.t>;vector<keyword>;datascript.db/tx-entry>)

(signature datascript.db/tx-call
  :fn<fn<datascript.db/DB;vector<datascript.db/tx-entry>>;datascript.db/tx-entry>)

(signature datascript.db/tx-invalid
  :fn<string;datascript.db/tx-entry>)

(signature datascript.db/tx-add
  :fn<Datascript_runtime.Data_value.entity_ref;keyword;Datascript_runtime.Data_value.t;datascript.db/tx-entry>)

(signature datascript.db/tx-add-with-tx
  :fn<Datascript_runtime.Data_value.entity_ref;keyword;Datascript_runtime.Data_value.t;int;datascript.db/tx-entry>)

(signature datascript.db/tx-retract-entity-id
  :fn<int;datascript.db/tx-entry>)

(signature datascript.db/tx-retract-attribute
  :fn<Datascript_runtime.Data_value.entity_ref;keyword;datascript.db/tx-entry>)

(signature datascript.db/tx-cas
  :fn<Datascript_runtime.Data_value.entity_ref;keyword;option<Datascript_runtime.Data_value.t>;Datascript_runtime.Data_value.t;datascript.db/tx-entry>)

(signature datascript.db/tx-cas-operation
  :fn<keyword;Datascript_runtime.Data_value.entity_ref;keyword;option<Datascript_runtime.Data_value.t>;Datascript_runtime.Data_value.t;datascript.db/tx-entry>)

(signature datascript.db/tx-retract
  :fn<Datascript_runtime.Data_value.entity_ref;keyword;Datascript_runtime.Data_value.t;datascript.db/tx-entry>)

(signature datascript.db/tx-retract-option
  :fn<Datascript_runtime.Data_value.entity_ref;keyword;option<Datascript_runtime.Data_value.t>;datascript.db/tx-entry>)

(signature datascript.db/tx-retract-entity
  :fn<Datascript_runtime.Data_value.entity_ref;datascript.db/tx-entry>)

(signature datascript.db/tx-set-tuple
  :fn<Datascript_runtime.Data_value.entity_ref;keyword;option<Datascript_runtime.Data_value.t>;datascript.db/tx-entry>)

(signature datascript.db/with-db-metadata
  :fn<datascript.db/DB;map<keyword;Datascript_runtime.Data_value.t>;datascript.db/DB>)

(defn with-db-metadata
  [database
   metadata]
  (assoc database :metadata metadata))

(signature datascript.db/db-metadata
  :fn<datascript.db/DB;map<keyword;Datascript_runtime.Data_value.t>>)

(defn db-metadata
  [database]
  (.-metadata database))

(type-record database-diff
  (only-left :option<vector<Datom>>)
  (only-right :option<vector<Datom>>)
  (both :option<vector<Datom>>))

(signature datascript.db/non-empty-datoms
  :fn<vector<datascript.db/Datom>;option<vector<datascript.db/Datom>>>)

(signature datascript.db/append-datoms-from
  :fn<vector<datascript.db/Datom>;vector<datascript.db/Datom>;int;vector<datascript.db/Datom>>)

(defn non-empty-datoms
  [ datoms]
  (if (empty? datoms)
    None
    (Some datoms)))

(defn  append-datoms-from
  [ result
    source
    start-index]
  (loop [result result
         index start-index]
    (if (< index (count source))
      (recur (conj result (nth source index)) (inc index))
      result)))

(signature datascript.db/diff-databases
  :fn<datascript.db/DB;datascript.db/DB;datascript.db/database-diff>)
(defn diff-databases
  [left right]
  (let [left-datoms
        (vec (set/set-seq (.-eavt left)))
        right-datoms
        (vec (set/set-seq (.-eavt right)))]
    (loop [left-index 0
           right-index 0
           only-left []
           only-right []
           both []]
      (cond
        (= left-index (count left-datoms))
        (record database-diff
          (only-left (non-empty-datoms only-left))
          (only-right
           (non-empty-datoms
            (append-datoms-from
             only-right right-datoms right-index)))
          (both (non-empty-datoms both)))

        (= right-index (count right-datoms))
        (record database-diff
          (only-left
           (non-empty-datoms
            (append-datoms-from
             only-left left-datoms left-index)))
          (only-right (non-empty-datoms only-right))
          (both (non-empty-datoms both)))

        :else
        (let [left-datom (nth left-datoms left-index)
              right-datom (nth right-datoms right-index)
              comparison
              (cmp-datoms-eav-quick left-datom right-datom)]
          (cond
            (= comparison 0)
            (recur
             (inc left-index)
             (inc right-index)
             only-left
             only-right
             (conj both left-datom))

            (< comparison 0)
            (recur
             (inc left-index)
             right-index
             (conj only-left left-datom)
             only-right
             both)

            :else
            (recur
             left-index
             (inc right-index)
             only-left
             (conj only-right right-datom)
             both)))))))

(defn validate-indexed
  [db
   index
   c0
   c1
   c2
   c3]
  (when (= index :avet)
    (when-some [attr (index-component-keyword c0)]
      (when-not (contains? (-attrs-by db :db/index) attr)
        (util/raise "Attribute " attr " should be marked as :db/index true"
                    {:error :index-access
                     :index :avet
                     :components (tuple c0 c1 c2 c3)})))))

(defn db? [x]
  (and (satisfies? ISearch x)
       (satisfies? IIndexAccess x)
       (satisfies? IDB x)))

;; ----------------------------------------------------------------------------
(defrecord FilteredDB [^datascript.db/DB unfiltered-db
                       ^:fn<datascript.db/Datom;bool> pred
                       ^:int identity-hash
                       ^:ref<int> hash]
  IDB
  (-schema [db]
           (-schema (.-unfiltered-db db)))

  (-attrs-by [db property]
             (-attrs-by (.-unfiltered-db db) property))

  (-attr-tuples [db]
                (-attr-tuples (.-unfiltered-db db)))

  (-unfiltered-db [db]
                  (.-unfiltered-db db))

  ISearch
  (-search [db pattern]
           (filter (.-pred db) (-search (.-unfiltered-db db) pattern)))

  IIndexAccess
  (-datoms [db index c0 c1 c2 c3]
           (filter (.-pred db) (-datoms (.-unfiltered-db db) index c0 c1 c2 c3)))

  (-seek-datoms [db index c0 c1 c2 c3]
                (filter (.-pred db) (-seek-datoms (.-unfiltered-db db) index c0 c1 c2 c3)))

  (-rseek-datoms [db index c0 c1 c2 c3]
                 (filter (.-pred db) (-rseek-datoms (.-unfiltered-db db) index c0 c1 c2 c3)))

  (-index-range [db attr start end]
                (filter (.-pred db) (-index-range (.-unfiltered-db db) attr start end))))

(defn database-datom-print-string [datom]
  (str
   "["
   (.-e datom)
   " "
   (.-a datom)
   " "
   (Datascript_runtime.Data_value.to_edn_string (.-v datom))
   " "
   (datom-tx datom)
   "]"))

(defn database-parts-print-string
  [schema
   datoms]
  (str
   "#datascript/DB {:schema "
   (match schema
     None "{}"
     (Some _)
     (Datascript_runtime.Serialization_value.schema_to_string
      schema))
   ", :datoms ["
   (reduce
    (fn [result datom]
      (let [printed (database-datom-print-string datom)]
        (if (empty? result)
          printed
          (str result " " printed))))
    ""
    datoms)
   "]}"))

(defn database-print-string [database]
  (database-parts-print-string
   (-schema database)
   (-datoms database :eavt nil nil nil nil)))

(defn pr-db [database writer _opts]
  (Buffer.add_string writer (database-print-string database)))

(defn filtered-database-print-string
  [database]
  (database-parts-print-string
   (.-schema (.-unfiltered-db database))
   (-datoms database :eavt nil nil nil nil)))

(defmethod print-method DB
  [database writer]
  (Buffer.add_string writer (database-print-string database)))

(defmethod print-method FilteredDB
  [database writer]
  (Buffer.add_string writer (filtered-database-print-string database)))

(type-variant database-view
  (DatabaseView :datascript.db/DB)
  (FilteredDatabaseView :datascript.db/FilteredDB))

(defprotocol IDatabaseView
  (-database-view [db] :datascript.db/database-view))

(extend-type DB
  IDatabaseView
  (-database-view [db] (DatabaseView db)))

(extend-type FilteredDB
  IDatabaseView
  (-database-view [db] (FilteredDatabaseView db)))

(extend-type database-view
  IDatabaseView
  (-database-view [database] database))

(defn database-view [database]
  (-database-view database))

(defprotocol IFilteredView
  (-filtered? [db] :bool)
  (-filter-view
   [db  pred]
   :datascript.db/FilteredDB))

(extend-type DB
  IFilteredView
  (-filtered? [_db] false)
  (-filter-view
    [db  view-pred]
    (FilteredDB.
     db
     (fn [datom]
       (view-pred db datom))
     (fresh-db-identity)
     (atom 0))))

(extend-type FilteredDB
  IFilteredView
  (-filtered? [_db] true)
  (-filter-view
    [filtered-db
      view-pred]
    (let [original-pred (.-pred filtered-db)
          original-db (.-unfiltered-db filtered-db)]
      (FilteredDB.
       original-db
       (fn [datom]
         (and
          (original-pred datom)
          (view-pred original-db datom)))
       (fresh-db-identity)
       (atom 0)))))

(defn- search-pattern
  [ e
    a
    v
    tx]
  (datom-bound e a v tx e0 tx0))

(defn-  fsearch
  [ data
    e
    a
   v
    tx]
  (first (-search data (search-pattern e a v tx))))

(defn  search-ea
  [ data  eid  attr]
  (fsearch data (Some eid) (Some attr) None None))

(defn unfiltered-db  [db]
  (-unfiltered-db db))

(signature datascript.db/db-equal?
  :fn<datascript.db/DB;datascript.db/DB;bool>)
(defn db-equal?
  [left right]
  (and
   (= (.-schema left) (.-schema right))
   (datom-vectors-equal?
    (vec (set/set-seq (.-eavt left)))
    (vec (set/set-seq (.-eavt right))))))

(defn  datom-hash [datom]
  (Hashtbl.hash
   (tuple
    (.-e datom)
    (.-a datom)
    (Datascript_runtime.Data_value.hash (.-v datom)))))

(defn  schema-entry-value
  [ entry]
  (Datascript_runtime.Data_value.map_of_keyword_map entry))

(defn  schema-hash
  [ schema]
  (Datascript_runtime.Data_value.hash
   (Datascript_runtime.Data_value.map_of_keyword_map_with
    schema-entry-value
    schema)))

(defn db-hash [database]
  (let [cached @(:hash database)]
    (if (zero? cached)
      (let [value
            (reduce
             (fn [value datom]
               (Hashtbl.hash (tuple value (datom-hash datom))))
             (schema-hash (effective-schema (:schema database)))
             (set/set-seq (.-eavt database)))]
        (reset! (:hash database) value)
        value)
      cached)))

(signature datascript.db/db-count
  :fn<datascript.db/DB;int>)
(defn db-count [database]
  (count (:eavt database)))

(defn  filtered-db-datoms
  [ database]
  (vec (-datoms database :eavt nil nil nil nil)))

(defn filtered-db-equal?
  [ left
    right]
  (and
   (= (-schema left) (-schema right))
   (datom-vectors-equal?
    (filtered-db-datoms left)
    (filtered-db-datoms right))))

(signature datascript.db/filtered-db-hash
  :fn<datascript.db/FilteredDB;int>)
(defn filtered-db-hash
  [database]
  (let [cached @(:hash database)]
    (if (zero? cached)
      (let [value
            (reduce
             (fn [value datom]
               (Hashtbl.hash (tuple value (datom-hash datom))))
             (schema-hash (effective-schema (-schema database)))
             (filtered-db-datoms database))]
        (reset! (:hash database) value)
        value)
      cached)))

(defn  filtered-db-count
  [ database]
  (count (filtered-db-datoms database)))

(extend-type DB
  IEquiv
  (-equiv [ db  other]
    (db-equal? db other))

  IHash
  (-hash [ db] (db-hash db))

  ICounted
  (-count [ db] (db-count db)))

(extend-type FilteredDB
  IEquiv
  (-equiv
    [ db
      other]
    (filtered-db-equal? db other))

  IHash
  (-hash [ db] (filtered-db-hash db))

  ICounted
  (-count [ db] (filtered-db-count db)))

(signature datascript.db/db-transient
  :fn<datascript.db/DB;datascript.db/DB>)
(defn  db-transient
  [database]
  (assoc
   database
   :eavt (-as-transient (:eavt database))
   :aevt (-as-transient (:aevt database))
   :avet (-as-transient (:avet database))))

(signature datascript.db/db-persistent!
  :fn<datascript.db/DB;datascript.db/DB>)
(defn  db-persistent!
  [database]
  (assoc
   database
   :eavt (-persistent! (:eavt database))
   :aevt (-persistent! (:aevt database))
   :avet (-persistent! (:avet database))))

;; ----------------------------------------------------------------------------

(signature datascript.db/attr->properties
  :fn<keyword;Datascript_runtime.Data_value.t;vector<keyword>>)

(defn  attr->properties
  [ k  v]
  (if-some [value (Datascript_runtime.Data_value.keyword_value v)]
    (case value
      ":db.unique/identity"  [:db/unique :db.unique/identity :db/index]
      ":db.unique/value"     [:db/unique :db.unique/value :db/index]
      ":db.cardinality/many" [:db.cardinality/many]
      ":db.type/ref"         [:db.type/ref :db/index]
      [])
    (if-some [value (Datascript_runtime.Data_value.bool_value v)]
      (if value
        (cond
          (= :db/isComponent k) [:db/isComponent]
          (= :db/index k)       [:db/index]
          :else                 [])
        [])
      (if (= :db/tupleAttrs k)
        [:db.type/tuple :db/index]
        []))))

(signature datascript.db/attr-tuples
  :fn<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>;map<keyword;set<keyword>>;map<keyword;map<keyword;int>>>)

(defn  attr-tuples
  "e.g. :reg/semester => #{:reg/semester+course+student ...}"
  [ schema
    rschema]
  (reduce
   (fn [m tuple-attr] ;; e.g. :reg/semester+course+student
     (let [attrs
           (if-some
            [attrs
             (Datascript_runtime.Data_value.keyword_items
             (get
               (get schema tuple-attr empty-schema-entry)
               :db/tupleAttrs
               value-wildcard))]
             (mapv (fn [attr] (keyword attr)) attrs)
             [])]
       (reduce-kv
        (fn [m idx src-attr]
          (assoc
           m
           src-attr
           (assoc (get m src-attr {}) tuple-attr idx)))
        m
        attrs)))
   {}
   (:db.type/tuple rschema)))

(signature datascript.db/rschema
  :fn<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>;datascript.db/ReverseSchema>)

(signature datascript.db/add-rschema-attr
  :fn<option<set<keyword>>;keyword;set<keyword>>)

(defn- add-rschema-attr [attrs attr]
  (if-some [attrs attrs]
    (conj attrs attr)
    #{attr}))

(defn- rschema
  [ schema]
  ":db/unique           => #{attr ...}
   :db.unique/identity  => #{attr ...}
   :db.unique/value     => #{attr ...}
   :db/index            => #{attr ...}
   :db.cardinality/many => #{attr ...}
   :db.type/ref         => #{attr ...}
   :db/isComponent      => #{attr ...}
   :db.type/tuple       => #{attr ...}
  :db/attrTuples       => {attr => {tuple-attr => idx}}"
  (let [rschema (reduce-kv
                 (fn [m attr keys->values]
                   (reduce-kv
                    (fn [m key value]
                      (reduce
                       (fn [m prop]
                         (update
                         m prop
                          (fn [attrs]
                            (add-rschema-attr attrs attr))))
                       m (attr->properties key value)))
                    (update
                     m
                     :db/ident
                     (fn [attrs]
                       (add-rschema-attr attrs attr)))
                    keys->values))
                 {} schema)]
    (ReverseSchema.
     (get rschema :db/unique #{})
     (get rschema :db.unique/identity #{})
     (get rschema :db.unique/value #{})
     (get rschema :db/index #{})
     (get rschema :db.cardinality/many #{})
     (get rschema :db.type/ref #{})
     (get rschema :db/isComponent #{})
     (get rschema :db.type/tuple #{})
     (attr-tuples schema rschema))))

(signature datascript.db/invalid-schema-value
  :fn<keyword;keyword;unit>)

(defn- invalid-schema-value [ a  k]
  (Stdlib.invalid_arg
   (str "Bad attribute specification for " a
        ": " k " has an invalid value")))

(signature datascript.db/schema-bool-value-description
  :fn<Datascript_runtime.Data_value.t;string>)

(defn-  schema-bool-value-description
  [ value]
  (match value
    (Datascript_runtime.Data_value.String string-value)
    (str "\"" string-value "\"")
    _ "<unsupported value>"))

(signature datascript.db/invalid-schema-bool-value
  :fn<keyword;keyword;Datascript_runtime.Data_value.t;unit>)

(defn- invalid-schema-bool-value
  [ attr
    key
    value]
  (Stdlib.invalid_arg
   (str
    "Bad attribute specification for {"
    attr " {" key " " (schema-bool-value-description value)
    "}}, expected one of #{true false}")))

(signature datascript.db/schema-keyword
  :fn<map<keyword;Datascript_runtime.Data_value.t>;keyword;option<keyword>>)

(defn-  schema-keyword
  [ entry  key]
  (if-some [value (get entry key)]
    (Datascript_runtime.Data_value.keyword_value value)
    None))

(signature datascript.db/schema-bool
  :fn<map<keyword;Datascript_runtime.Data_value.t>;keyword;option<bool>>)

(defn-  schema-bool
  [ entry  key]
  (if-some [value (get entry key)]
    (Datascript_runtime.Data_value.bool_value value)
    None))

(signature datascript.db/validate-schema-keyword
  :fn<keyword;keyword;map<keyword;Datascript_runtime.Data_value.t>;set<keyword>;unit>)

(defn- validate-schema-keyword
  [ a
    k
    entry
    expected]
  (when-some [value (get entry k)]
    (if-some [keyword-value
              (Datascript_runtime.Data_value.keyword_value value)]
      (when-not (contains? expected (keyword keyword-value))
        (invalid-schema-value a k))
      (invalid-schema-value a k))))

(signature datascript.db/validate-schema-bool
  :fn<keyword;keyword;map<keyword;Datascript_runtime.Data_value.t>;unit>)

(defn- validate-schema-bool
  [ a
    k
    entry]
  (when-some [value (get entry k)]
    (when-not
      (some? (Datascript_runtime.Data_value.bool_value value))
      (invalid-schema-bool-value a k value))))

(signature datascript.db/validate-tuple-schema
  :fn<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>;keyword;map<keyword;Datascript_runtime.Data_value.t>;unit>)

(defn- validate-tuple-schema
  [ schema
    a
    entry]
  (when-some [tuple-value (get entry :db/tupleAttrs)]
    (when (= (schema-keyword entry :db/cardinality)
             (Some :db.cardinality/many))
      (Stdlib.invalid_arg
       (str a " has :db/tupleAttrs, must be :db.cardinality/one")))

    (if-some [attrs
              (Datascript_runtime.Data_value.keyword_items tuple-value)]
      (do
        (when (empty? attrs)
          (Stdlib.invalid_arg
           (str a " :db/tupleAttrs can’t be empty")))

        (doseq [attr-name attrs]
          (let [attr (keyword attr-name)
                dependency (get schema attr empty-schema-entry)]
            (when (contains? dependency :db/tupleAttrs)
              (Stdlib.invalid_arg
               (str a
                    " :db/tupleAttrs can’t depend on another tuple attribute: "
                    attr)))

            (when (= (schema-keyword dependency :db/cardinality)
                     (Some :db.cardinality/many))
              (Stdlib.invalid_arg
               (str a
                    " :db/tupleAttrs can’t depend on :db.cardinality/many attribute: "
                    attr))))))
      (Stdlib.invalid_arg
       (str
        a
        " :db/tupleAttrs must be a sequential collection, got: "
        (Datascript_runtime.Data_value.to_edn_string tuple-value))))))

(signature datascript.db/validate-schema
  :fn<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>;unit>)

(defn- validate-schema
  [schema]
  (reduce-kv
   (fn [_ a entry]
     (validate-schema-bool a :db/isComponent entry)
     (validate-schema-bool a :db/index entry)
     (validate-schema-bool a :db/noHistory entry)
     (validate-schema-keyword
      a :db/unique entry #{:db.unique/value :db.unique/identity})
     (validate-schema-keyword a :db/valueType entry ds/type?)
     (validate-schema-keyword
      a :db/cardinality entry
      #{:db.cardinality/one :db.cardinality/many})

     (when (= (schema-bool entry :db/isComponent) (Some true))
       (when-not (= (schema-keyword entry :db/valueType)
                    (Some :db.type/ref))
         (Stdlib.invalid_arg
          (str
           "Bad attribute specification for " a
           ": {:db/isComponent true} should also have "
           "{:db/valueType :db.type/ref}"))))

     (when (= (schema-keyword entry :db/valueType)
              (Some :db.type/tuple))
       (when-not (contains? entry :db/tupleAttrs)
         (Stdlib.invalid_arg
          (str "Bad attribute specification for " a
               ": {:db/valueType :db.type/tuple} should also have :db/tupleAttrs"))))

     (validate-tuple-schema schema a entry)
     (Stdlib.ignore 0))
   (Stdlib.ignore 0)
   schema))

(type-record database-options
  (storage :option<Datascript_runtime.Storage_backend.t>)
  (ref-type :Lg_runtime.Runtime_ref_type.t)
  (branching-factor :int))

(defn  default-options []
  (record database-options
          (storage None)
          (ref-type (Lg_runtime.Runtime_ref_type.Weak))
          (branching-factor 32)))

(defn  options-with-storage
  [ storage]
  (record database-options
          (storage (Some storage))
          (ref-type (Lg_runtime.Runtime_ref_type.Weak))
          (branching-factor 32)))

(defn  options-assoc-storage
  [ opts
    storage]
  (record database-options
          (storage (Some storage))
          (ref-type (.-ref-type opts))
          (branching-factor (.-branching-factor opts))))

(defn  options-with-ref-type
  [ opts  ref-type]
  (record database-options
          (storage (.-storage opts))
          (ref-type ref-type)
          (branching-factor (.-branching-factor opts))))

(defn  options-with-branching-factor
  [ opts  branching-factor]
  (when (< branching-factor 2)
    (Stdlib.invalid_arg "Branching factor must be at least 2"))
  (record database-options
          (storage (.-storage opts))
          (ref-type (.-ref-type opts))
          (branching-factor branching-factor)))

(signature datascript.db/options-storage
  :fn<datascript.db/database-options;option<Datascript_runtime.Storage_backend.t>>)

(defn options-storage
  [ opts]
  (.-storage opts))

(defn  options-ref-type
  [ opts]
  (.-ref-type opts))

(defn  options-branching-factor [ opts]
  (.-branching-factor opts))

(defn- empty-datom-set [comparator  opts]
  (set/with-branching-factor
   (set/with-ref-type
    (set/sorted-set-with-comparator
     comparator
     None)
    (options-ref-type opts))
   (options-branching-factor opts)))

(defn  empty-db
  [ maybe-schema
    opts]
  (let [schema (effective-schema maybe-schema)]
  (validate-schema schema)
  (record DB
          (schema maybe-schema)
          (schema-idents empty-schema-idents)
          (schema-drafts empty-schema-drafts)
          (eavt (empty-datom-set cmp-datoms-eavt opts))
          (aevt (empty-datom-set cmp-datoms-aevt opts))
          (avet (empty-datom-set cmp-datoms-avet opts))
          (max-eid e0)
          (max-tx tx0)
          (rschema (rschema (merge-schema implicit-schema schema)))
          (tx-functions {})
          (identity-hash (fresh-db-identity))
          (hash (atom 0))
          (metadata {}))))

(defn- init-max-eid [rschema eavt avet]
  (let [max     (fn [current candidate]
                  (if-some [candidate candidate]
                    (if (> candidate current) candidate current)
                    current))
        max-eid (some->
                 (set/rslice eavt
                             (datom (dec tx0) attr-wildcard value-wildcard txmax)
                             (datom e0 attr-wildcard value-wildcard tx0))
                 first :e)
        res     (max e0 max-eid)
        max-ref (fn [attr]
                  (if-some
                   [datom
                    (first
                     (set/rslice
                      avet
                      (datom
                       (dec tx0)
                       attr
                       (Datascript_runtime.Data_value.Ref (dec tx0))
                       txmax)
                      (datom
                       e0
                       attr
                       (Datascript_runtime.Data_value.Ref e0)
                       tx0)))]
                    (Datascript_runtime.Data_value.ref_value (:v datom))
                    None))
        refs    (:ref-attrs rschema)
        res     (reduce
                 (fn [res attr]
                   (max res (max-ref attr)))
                 res refs)]
    res))

(signature datascript.db/init-max-tx
  :fn<array<datascript.db/Datom>;int>)

(defn- init-max-tx [datoms]
  (loop [index 0
         result tx0]
    (if (< index (arrays/alength datoms))
      (recur
       (inc index)
       (max result (datom-tx (arrays/aget datoms index))))
      result)))

(defn-  datom-set-from-sorted-array
  [comparator
    datoms
    length
    opts]
  (set/with-branching-factor
   (set/from-sorted-array
    comparator
    datoms
    length
    None
    (options-ref-type opts))
   (options-branching-factor opts)))

(defn  normalize-init-datom
  [ reverse-schema source]
  (if
    (Lg_runtime.Core_set.String_set.mem
     (datom-attr source)
     (:ref-attrs reverse-schema))
    (match (.-v source)
      (Datascript_runtime.Data_value.Int eid)
      (datom-closed
       (.-e source)
       (datom-attr source)
       (Datascript_runtime.Data_value.Ref eid)
       (datom-tx source)
       (datom-added source))
      _ source)
    source))

(defn- indexed-datoms-array
  [indexed datoms]
  (let [length (arrays/alength datoms)
        indexed-count
        (Lg_runtime.Core_set.String_set.cardinal indexed)]
    (if (or (= length 0) (= indexed-count 0))
      (arrays/empty-array)
      (let [single-indexed
            (if (= indexed-count 1)
              (Some (Lg_runtime.Core_set.String_set.min_elt indexed))
              None)
            output
            (arrays/make-array
             length
             (arrays/aget datoms 0))]
        (loop [source-index 0
               target-index 0]
          (if (< source-index length)
            (let [datom (arrays/aget datoms source-index)]
              (if
               (match single-indexed
                 (Some attr) (= (datom-attr datom) attr)
                 None
                 (Lg_runtime.Core_set.String_set.mem
                  (datom-attr datom)
                  indexed))
                (do
                  (arrays/aset output target-index datom)
                  (recur
                   (inc source-index)
                   (inc target-index)))
                (recur (inc source-index) target-index)))
            (if (= target-index length)
              output
              (arrays/aslice output 0 target-index))))))))

(defn init-db-with-schema-option
  ([ datoms
     schema]
   (init-db-with-schema-option datoms schema (default-options)))
  ([ datoms
     maybe-schema
     opts]
  (let [schema (effective-schema maybe-schema)]
  (validate-schema schema)
  (let [rschema     (rschema (merge-schema implicit-schema schema))
        indexed     (:indexed-attrs rschema)
        arr         datoms
        _           (if
                      (Lg_runtime.Core_set.String_set.is_empty
                       (:ref-attrs rschema))
                      (Stdlib.ignore 0)
                      (loop [index 0]
                        (if (< index (arrays/alength arr))
                          (let [source (arrays/aget arr index)
                                normalized
                                (normalize-init-datom rschema source)]
                            (when-not (identical? source normalized)
                              (arrays/aset arr index normalized))
                            (recur (inc index)))
                          (Stdlib.ignore 0))))
        _           (arrays/asort arr cmp-datoms-eavt-quick)
        eavt        (datom-set-from-sorted-array
                     cmp-datoms-eavt arr (arrays/alength arr) opts)
        _           (arrays/asort arr cmp-datoms-aevt-quick)
        aevt        (datom-set-from-sorted-array
                     cmp-datoms-aevt arr (arrays/alength arr) opts)
        avet-arr    (indexed-datoms-array indexed arr)
        _           (arrays/asort avet-arr cmp-datoms-avet-quick)
        avet        (datom-set-from-sorted-array
                     cmp-datoms-avet avet-arr (arrays/alength avet-arr) opts)
        max-eid     (init-max-eid rschema eavt avet)
        max-tx      (init-max-tx arr)]
    (DB.
     maybe-schema
     empty-schema-idents
     empty-schema-drafts
     eavt
     aevt
     avet
     max-eid
     max-tx
     rschema
     {}
     (fresh-db-identity)
     (atom 0)
     {})))))

(defn  init-db
  ([ datoms
     schema]
   (init-db-with-schema-option datoms (Some schema)))
  ([ datoms
     schema
     opts]
   (init-db-with-schema-option datoms (Some schema) opts)))

(defn  db-from-reader
  [ value]
  (let [datoms
        (to-array
         (map
          datom-from-reader
          (Datascript_runtime.Serialization_value.reader_database_datoms value)))]
    (match
     (Datascript_runtime.Serialization_value.reader_database_schema value)
     (Some schema) (init-db datoms schema)
     None (init-db-with-schema-option datoms None))))

(defn  db-from-edn-string [ source]
  (db-from-reader
   (Datascript_runtime.Serialization_value.read_database source)))

(type-record db-snapshot
  (schema :option<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>>)
  (schema-idents :option<map<int;keyword>>)
  (schema-drafts :option<map<int;map<keyword;Datascript_runtime.Data_value.t>>>)
  (rschema :option<datascript.db/ReverseSchema>)
  (eavt :set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>>)
  (aevt :set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>>)
  (avet :set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>>)
  (max-eid :option<int>)
  (max-tx :option<int>))

(defn  make-db-snapshot
  [ schema
    eavt
    aevt
    avet
    max-eid
    max-tx]
  (record db-snapshot
          (schema schema)
          (schema-idents None)
          (schema-drafts None)
          (rschema None)
          (eavt eavt)
          (aevt aevt)
          (avet avet)
          (max-eid (Some max-eid))
          (max-tx (Some max-tx))))

(defn  restore-db
  [ snapshot]
  (let [schema (:schema snapshot)
        schema-map (effective-schema schema)]
    (DB.
     schema
     (if-some [idents (:schema-idents snapshot)]
       idents
       empty-schema-idents)
     (if-some [drafts (:schema-drafts snapshot)]
       drafts
       empty-schema-drafts)
     (:eavt snapshot)
     (:aevt snapshot)
     (:avet snapshot)
     (if-some [max-eid (:max-eid snapshot)] max-eid e0)
     (if-some [max-tx (:max-tx snapshot)] max-tx tx0)
     (if-some [reverse-schema (:rschema snapshot)]
       reverse-schema
       (rschema (merge-schema implicit-schema schema-map)))
     {}
     (fresh-db-identity)
     (atom 0)
     {})))

(defn  restore-db-from-storage
  [ schema
    eavt
    aevt
    avet
    max-eid
    max-tx]
  (restore-db
   (make-db-snapshot schema eavt aevt avet max-eid max-tx)))

(signature datascript.db/with-schema
  :fn<datascript.db/DB;map<keyword;map<keyword;Datascript_runtime.Data_value.t>>;datascript.db/DB>)
(defn with-schema
  [db
   schema]
  (assoc db
         :schema        (Some schema)
         :schema-idents empty-schema-idents
         :schema-drafts empty-schema-drafts
         :rschema       (rschema (merge-schema implicit-schema schema))
         :identity-hash (fresh-db-identity)
         :hash          (atom 0)))

(defn- typed-index
  [db index]
  (case index
    :eavt (.-eavt db)
    :aevt (.-aevt db)
    :avet (.-avet db)
    (util/raise "Unknown index " index)))

(signature datascript.db/reduce-eavt-slice
  [result]
  :fn<datascript.db/DB;int;keyword;option<Datascript_runtime.Data_value.t>;fn<result;Datom;result>;result;result>)

(defn reduce-eavt-slice
  [db
    entity
    attr
    value
   f
   initial]
  (let [eavt (.-eavt db)
        comparator (set/comparator eavt)]
    (set/set-slice-reduce-with
     eavt
     (datom-bound
      (Some entity) (Some attr) value None e0 tx0)
     (datom-bound
      (Some entity) (Some attr) value None e0 txmax)
     comparator
     f
     initial)))

(defn search-vector
  [db
   e
   a
   v
   tx]
  (match (tuple e a v tx)
    (tuple (Some entity) (Some attr) value None)
    (reduce-eavt-slice
     db entity attr value
     (fn [datoms datom]
       (conj datoms datom))
     [])

    (tuple None (Some attr) (Some value) None)
    (if (contains? (-attrs-by db :db/index) attr)
      (set/set-slice-reduce-with
       (.-avet db)
       (datom-bound None (Some attr) (Some value) None e0 tx0)
       (datom-bound None (Some attr) (Some value) None emax txmax)
       (set/comparator (.-avet db))
       (fn [datoms datom]
         (conj datoms datom))
       [])
      (set/set-slice-reduce-with
       (.-aevt db)
       (datom-bound None (Some attr) None None e0 tx0)
       (datom-bound None (Some attr) None None emax txmax)
       (set/comparator (.-aevt db))
       (fn [datoms datom]
         (if (let [candidate (.-v datom)]
               (match value
                 (Datascript_runtime.Data_value.String expected)
                 (match candidate
                   (Datascript_runtime.Data_value.String actual)
                   (String.equal actual expected)
                   _ false)
                 (Datascript_runtime.Data_value.Keyword expected)
                 (match candidate
                   (Datascript_runtime.Data_value.Keyword actual)
                   (String.equal actual expected)
                   _ false)
                 _
                 (Datascript_runtime.Data_value.equal candidate value)))
           (conj datoms datom)
           datoms))
       []))

    (tuple None (Some attr) None None)
    (set/set-slice-reduce-with
     (.-aevt db)
     (datom-bound None (Some attr) None None e0 tx0)
     (datom-bound None (Some attr) None None emax txmax)
     (set/comparator (.-aevt db))
     (fn [datoms datom]
       (conj datoms datom))
     [])

    _
    (vec (-search db (search-pattern e a v tx)))))

;; ----------------------------------------------------------------------------

(declare entid-strict ref?)

(defn resolve-datom
  [db
   e
   a
   v
   tx
   default-e
   default-tx]
  (if-some [attr a]
    (validate-attr attr (tuple attr v))
    nil)
  (let [resolved-e
        (if-some [entity-ref e]
          (Some (entid-strict db entity-ref))
          None)
        resolved-v
        (if-some [value v]
          (if-some [attr a]
            (if (ref? db attr)
              (if-some
               [entity-ref
                (Datascript_runtime.Data_value.entity_ref_value value)]
                (Some
                 (Datascript_runtime.Data_value.Ref
                  (entid-strict db entity-ref)))
                (raise
                 (Invalid_argument
                  "Reference attribute value must be an entity reference")))
              (Some value))
            (Some value))
          None)
        resolved-tx
        (if-some [transaction-ref tx]
          (Some (entid-strict db transaction-ref))
          None)]
    (datom-bound
     resolved-e a resolved-v resolved-tx default-e default-tx)))

(defn components->pattern
  [db
   index
   c0
   c1
   c2
   c3
   default-e
   default-tx]
  (case index
    :eavt (resolve-datom
           db
           (index-component-entity-ref c0)
           (index-component-keyword c1)
           c2
           (index-component-entity-ref c3)
           default-e
           default-tx)
    :aevt (resolve-datom
           db
           (index-component-entity-ref c1)
           (index-component-keyword c0)
           c2
           (index-component-entity-ref c3)
           default-e
           default-tx)
    :avet (resolve-datom
           db
           (index-component-entity-ref c2)
           (index-component-keyword c0)
           c1
           (index-component-entity-ref c3)
           default-e
           default-tx)
    (Stdlib.invalid_arg (str "Unknown index " index))))

(defn find-datom
  [db index c0 c1 c2 c3]
  (validate-indexed db index c0 c1 c2 c3)
  (let [set     (typed-index db index)
        cmp     (set/comparator set)
        from    (components->pattern db index c0 c1 c2 c3 e0 tx0)
        to      (components->pattern db index c0 c1 c2 c3 emax txmax)
        datom   (set/seek-first set from cmp)]
    (when (and (some? datom) (<= 0 (cmp to datom)))
      datom)))

;; ----------------------------------------------------------------------------

(defn  empty-entity-map []
  {})

(defn  empty-value-eids []
  {})

(defn  empty-upsert-map []
  {})

(def *last-auto-tempid (atom 0))

(defn  auto-tempid []
  (Datascript_runtime.Data_value.Auto_tempid
   (swap! *last-auto-tempid inc)))

(defn  auto-tempid?
  [ entity-ref]
  (match entity-ref
    (Datascript_runtime.Data_value.Auto_tempid _) true
    _ false))

(defn ordered-tx-entity
  [ entity
    attrs]
  (record tx-entity-form
    (values entity)
    (attrs attrs)))

(defn tx-entity
  [ entity]
  (TxEntity
   (ordered-tx-entity entity (vec (keys entity)))))

(defn tx-entity-ordered
  [ entity
    attrs]
  (TxEntity (ordered-tx-entity entity attrs)))

(defn tx-nil []
  (TxNil))

(defn tx-call
  [ callback]
  (TxCall callback))

(defn tx-install-function
  [ entity
    callback]
  (TxInstallFunction entity callback))

(defn tx-invoke-function
  [ ident
    arguments]
  (TxInvokeFunction ident arguments))

(def builtin-fn?
  #{:db.fn/call
    :db.fn/cas
    :db/cas
    :db/add
    :db/retract
    :db.fn/retractAttribute
    :db.fn/retractEntity
    :db/retractEntity})

(defn tx-invalid [message]
  (TxInvalid message))

(defn tx-add
  [ entity
    attr
    value]
  (TxAdd entity attr value None))

(defn tx-add-with-tx
  [ entity
    attr
    value
    transaction]
  (TxAdd entity attr value (Some transaction)))

(defn tx-retract-entity-id [entity]
  (TxRetractEntity
   (Datascript_runtime.Data_value.Entity_id entity)))

(defn tx-retract-attribute
  [ entity  attr]
  (TxRetractAttribute entity attr))

(defn tx-cas
  [ entity
    attr
    old-value
    new-value]
  (TxCas :db.fn/cas entity attr old-value new-value))

(defn tx-cas-operation
  [ operation
    entity
    attr
    old-value
    new-value]
  (TxCas operation entity attr old-value new-value))

(defn datom->tx-entry [datom]
  (let [entity-ref
        (Datascript_runtime.Data_value.Entity_id (.-e datom))
        attr (datom-attr datom)
        value (.-v datom)]
    (if (datom-added datom)
      (TxAdd entity-ref attr value (Some (datom-tx datom)))
      (TxRetract entity-ref attr (Some value)))))

(defn tx-datom [datom]
  (datom->tx-entry datom))

(defn tx-retract
  [ entity-ref
    attr
    value]
  (TxRetract entity-ref attr (Some value)))

(defn tx-retract-option
  [ entity-ref
    attr
    value]
  (TxRetract entity-ref attr value))

(defn tx-retract-entity
  [ entity-ref]
  (TxRetractEntity entity-ref))

(defn tx-set-tuple
  [ entity-ref
    attr
    value]
  (TxSetTuple entity-ref attr value))

(defn tx-flush-tuples []
  (TxFlushTuples))

(defn tx-data
  {:inline
   (fn [source]
     (let [data-value
           (fn data-value [value]
             (if (map? value)
               (list
                'Datascript_runtime.Data_value.map_of_keyword_entries
                (list
                 'vec
                 (cons
                  'list
                  (map
                   (fn [entry]
                     (list
                      'tuple
                      (str (first entry))
                      (if (= (first entry) :db/id)
                        (let [entity (second entry)]
                          (list
                           'Datascript_runtime.Data_value.Ref_to
                           (if (string? entity)
                             (list
                              'Datascript_runtime.Data_value.Temp_id
                              entity)
                             (if (keyword? entity)
                               (list
                                'Datascript_runtime.Data_value.Ident
                                (str entity))
                               (if (vector? entity)
                                 (list
                                  'Datascript_runtime.Data_value.Lookup_ref
                                  (str (first entity))
                                  (data-value (second entity)))
                                 (if
                                  (or (symbol? entity) (seq? entity))
                                   entity
                                   (list
                                    'Datascript_runtime.Data_value.Entity_id
                                    entity)))))))
                        (data-value (second entry)))))
                   value))))
               (if (vector? value)
               (list
                'Datascript_runtime.Data_value.vector_of_vector
                (list
                 'vec
                 (cons
                  'list
                  (map
                   (fn [item]
                     (data-value item))
                   value))))
               (if (nil? value)
                 (list 'Datascript_runtime.Data_value.Nil)
                 (if (string? value)
                   (list
                    'Datascript_runtime.Data_value.String
                    value)
                   (if (keyword? value)
                     (list
                      'Datascript_runtime.Data_value.Keyword
                      (str value))
                     (if (= value true)
                       (list
                        'Datascript_runtime.Data_value.Bool
                        true)
                       (if (= value false)
                         (list
                          'Datascript_runtime.Data_value.Bool
                          false)
                         (if (float? value)
                           (list
                            'Datascript_runtime.Data_value.Float
                            value)
                         (if (or (symbol? value) (seq? value))
                           value
                           (list
                           'Datascript_runtime.Data_value.Int
                            value)))))))))))
           entity-ref
           (fn entity-ref [value]
             (if (string? value)
               (list
                'Datascript_runtime.Data_value.Temp_id
                value)
               (if (keyword? value)
                 (list
                  'Datascript_runtime.Data_value.Ident
                  (str value))
                 (if (vector? value)
                   (list
                    'Datascript_runtime.Data_value.Lookup_ref
                    (str (first value))
                    (data-value (second value)))
                   (if (or (symbol? value) (seq? value))
                     value
                     (list
                      'Datascript_runtime.Data_value.Entity_id
                      value))))))
           tx-entry
           (fn [entry]
             (if (nil? entry)
               (list 'datascript.db/tx-nil)
               (if (vector? entry)
               (let [operation (first entry)
                     entity (second entry)
                     attr (first (nnext entry))
                     value (second (nnext entry))
                     extra (first (nnext (nnext entry)))]
                 (cond
                   (= operation :db.fn/call)
                   (list
                    'datascript.db/tx-call
                    (list
                     'fn
                     (vec
                      (list
                       'transaction-db))
                     (cons
                      (second entry)
                      (cons
                       'transaction-db
                       (nnext entry)))))

                   (= operation :db/add)
                   (if (nil? extra)
                     (list
                      'datascript.db/tx-add
                      (entity-ref entity)
                      attr
                      (data-value value))
                     (list
                      'datascript.db/tx-add-with-tx
                      (entity-ref entity)
                      attr
                      (data-value value)
                      extra))

                   (= operation :db/retract)
                   (list
                    'datascript.db/tx-retract-option
                    (entity-ref entity)
                    attr
                    (if (nil? value)
                      'None
                      (list
                       'Some
                       (data-value value))))

                   (= operation :db.fn/retractAttribute)
                   (list
                    'datascript.db/tx-retract-attribute
                    (entity-ref entity)
                    attr)

                   (or
                    (= operation :db.fn/retractEntity)
                    (= operation :db/retractEntity))
                   (list
                    'datascript.db/tx-retract-entity
                    (entity-ref entity))

                   (or
                    (= operation :db.fn/cas)
                    (= operation :db/cas))
                   (list
                    'datascript.db/tx-cas-operation
                    operation
                    (entity-ref entity)
                    attr
                    (if (nil? value)
                      'None
                      (list
                       'Some
                       (data-value value)))
                    (data-value extra))

                   (keyword? operation)
                   (list
                    'datascript.db/tx-invoke-function
                    operation
                    (list
                     'vec
                     (cons
                      'list
                      (map data-value (next entry)))))

                   :else
                   (list
                    'datascript.db/tx-invalid
                    (str
                     "Unknown operation at " entry
                     ", expected :db/add, :db/retract, :db.fn/call, "
                     ":db.fn/retractAttribute, :db.fn/retractEntity "
                     "or an ident corresponding to an installed "
                     "transaction function (e.g. {:db/ident <keyword> "
                     ":db/fn <Ifn>}, usage of :db/ident requires "
                     "{:db/unique :db.unique/identity} in schema)"))))
               (if (map? entry)
                 (let [function-property
                       (first
                        (filter
                         (fn [property]
                           (= (first property) :db/fn))
                         entry))
                       properties
                       (filter
                        (fn [property]
                          (if (= (first property) :db/fn)
                            false
                            true))
                        entry)
                       entity-form
                       (list
                        'zipmap
                        (vec (map first properties))
                        (vec
                         (map
                          (fn [property]
                            (if (= (first property) :db/id)
                              (list
                               'Datascript_runtime.Data_value.Ref_to
                               (entity-ref (second property)))
                              (data-value (second property))))
                          properties)))]
                   (if (nil? function-property)
                     (list
                      'datascript.db/tx-entity-ordered
                      entity-form
                      (vec (map first properties)))
                     (list
                      'datascript.db/tx-install-function
                      entity-form
                      (second function-property))))
                 (if (seq? entry)
                   (let [constructor (first entry)]
                     (if
                      (or
                       (= constructor 'd/datom)
                       (= constructor 'db/datom)
                       (= constructor 'datascript.core/datom)
                       (= constructor 'datascript.db/datom))
                       (list 'datascript.db/tx-datom entry)
                       entry))
                   (if (symbol? entry)
                     entry
                 (list
                  'datascript.db/tx-invalid
                  (str
                   "Bad entity type at " entry
                   ", expected map or vector"))))))))]
       (if (vector? source)
         (list
          'vec
          (cons
           'list
           (map tx-entry source)))
         source)))}
  [ source]
  source)

(signature datascript.db/add-upsert-resolution
  :fn<option<tuple<int;keyword;Datascript_runtime.Data_value.t>>;keyword;Datascript_runtime.Data_value.t;int;option<tuple<int;keyword;Datascript_runtime.Data_value.t>>>)

(signature datascript.db/empty-upsert-resolution
  :fn<unit;option<tuple<int;keyword;Datascript_runtime.Data_value.t>>>)

(signature datascript.db/add-value-eids
  :fn<option<tuple<int;keyword;Datascript_runtime.Data_value.t>>;keyword;map<Datascript_runtime.Data_value.t;int>;option<tuple<int;keyword;Datascript_runtime.Data_value.t>>>)

(signature datascript.db/collect-upsert-resolution
  :fn<map<keyword;map<Datascript_runtime.Data_value.t;int>>;option<tuple<int;keyword;Datascript_runtime.Data_value.t>>>)

(defn add-upsert-resolution
  [ resolution
    attr
    value
    eid]
  (if-some [existing resolution]
    (let [existing-eid   (tuple-get existing 0)
          existing-attr  (tuple-get existing 1)
          existing-value (tuple-get existing 2)]
      (if (= existing-eid eid)
        resolution
        (Stdlib.invalid_arg
         (str
          "Conflicting upserts: ["
          existing-attr
          " "
          (Datascript_runtime.Data_value.to_edn_string existing-value)
          "] resolves to "
          (Stdlib.string_of_int existing-eid)
          ", but ["
          attr
          " "
          (Datascript_runtime.Data_value.to_edn_string value)
          "] resolves to "
          (Stdlib.string_of_int eid)))))
    (Some (tuple eid attr value))))

(defn empty-upsert-resolution []
  None)

(defn add-value-eids
  [ resolution
    attr
    value-eids]
  (reduce-kv
   (fn [resolution value eid]
     (add-upsert-resolution resolution attr value eid))
   resolution
   value-eids))

(defn collect-upsert-resolution
  [ upserts]
  (reduce-kv
   (fn [resolution attr value-eids]
     (add-value-eids resolution attr value-eids))
   (empty-upsert-resolution)
   upserts))

(defrecord TxReport
  [^datascript.db/DB db-before
   ^datascript.db/DB db-after
   ^:vector<Datom> tx-data
   ^:map<Datascript_runtime.Data_value.t;int> tempids
   ^:Datascript_runtime.Data_value.t tx-meta
   ^:map<int;map<keyword;vector<option<Datascript_runtime.Data_value.t>>>> queued-tuples
   ^:map<int;Datascript_runtime.Data_value.t> value-tempids
   ^:map<int;bool> used-tempid-eids])

(defn  tx-data-count [ report]
  (count (.-tx-data report)))

(defn  empty-used-tempid-eids []
  {})

(defn is-attr?
  [db attr property]
  (contains? (-attrs-by db property) attr))

(defn multival?
  [db attr]
  (is-attr? db attr :db.cardinality/many))

(defn multi-value?
  [db
   attr
   value]
  (and
   (is-attr? db attr :db.cardinality/many)
   (or
    (some? (Datascript_runtime.Data_value.sequential_items value))
    (some? (Datascript_runtime.Data_value.set_items value)))))

(defn ref? [db attr]
  (is-attr? db attr :db.type/ref))

(defn component? [db attr]
  (is-attr? db attr :db/isComponent))

(defn indexing? [db attr]
  (is-attr? db attr :db/index))

(defn tuple? [db attr]
  (is-attr? db attr :db.type/tuple))

(defn tuple-source? [db attr]
  (contains? (-attr-tuples db) attr))

(signature datascript.db/reverse-ref?
  :fn<keyword;bool>)
(defn reverse-ref? [attr]
  (= \_ (nth (name attr) 0)))

(defn reverse-ref [attr]
  (if (reverse-ref? attr)
    (keyword (namespace attr) (subs (name attr) 1))
    (keyword (namespace attr) (str "_" (name attr)))))

(signature datascript.db/schema-tuple-attrs
  :fn<datascript.db/DB;keyword;vector<string>>)

(defn schema-tuple-attrs
  [db tuple-attr]
  (if-some [entry (get (effective-schema (:schema db)) tuple-attr)]
    (if-some [value (get entry :db/tupleAttrs)]
      (if-some [attrs
                (Datascript_runtime.Data_value.keyword_items value)]
        attrs
        (raise (Invalid_argument
                "Schema tuple attrs must contain keywords")))
      (raise (Invalid_argument
              "Schema tuple attribute has no :db/tupleAttrs")))
    (raise (Invalid_argument
            "Unknown schema tuple attribute"))))

(declare resolve-tuple-refs)

(signature datascript.db/entid
  :fn<datascript.db/DB;Datascript_runtime.Data_value.entity_ref;option<int>>)

(defn entid
  [db
   entity-ref]
  {:pre [(db? db)]}
  (match entity-ref
    (Datascript_runtime.Data_value.Entity_id eid)
    (if (pos? eid)
      (if (> eid emax)
        (raise
         (Invalid_argument
          (str
           "Highest supported entity id is "
           (Stdlib.string_of_int emax)
           ", got "
           (Stdlib.string_of_int eid))))
        (Some eid))
      None)
    (Datascript_runtime.Data_value.Ident ident)
    (if-some [datom
              (fsearch db nil :db/ident
                       (Some (Datascript_runtime.Data_value.Keyword ident))
                       nil)]
      (Some (.-e datom))
      None)
    (Datascript_runtime.Data_value.Lookup_ref attr-name value)
    (let [attr (keyword attr-name)]
      (when-not (is-attr? db attr :db/unique)
        (raise
         (Invalid_argument
          (str
           "Lookup ref attribute should be marked as :db/unique: ["
           attr
           " "
           (Datascript_runtime.Data_value.to_edn_string value)
           "]"))))
      (if (tuple? db attr)
        (let [value (resolve-tuple-refs db attr value)]
          (if-some [datom (fsearch db nil attr (Some value) nil)]
            (Some (.-e datom))
            None))
        (if (Datascript_runtime.Data_value.is_nil value)
          None
          (if-some [datom (fsearch db nil attr (Some value) nil)]
            (Some (.-e datom))
            None))))
    Datascript_runtime.Data_value.Current_tx
    None
    (Datascript_runtime.Data_value.Temp_id _)
    None
    (Datascript_runtime.Data_value.Auto_tempid _)
    None))

(signature datascript.db/filtered-entid
  :fn<datascript.db/FilteredDB;Datascript_runtime.Data_value.entity_ref;option<int>>)

(defn  filtered-entid
  [ database
    entity-ref]
  (let [unfiltered (.-unfiltered-db database)]
    (match entity-ref
      (Datascript_runtime.Data_value.Entity_id _)
      (entid unfiltered entity-ref)

      (Datascript_runtime.Data_value.Ident ident)
      (if-some
        [datom
         (first
          (-search
           database
           (search-pattern
            None
            (Some :db/ident)
            (Some (Datascript_runtime.Data_value.Keyword ident))
            None)))]
        (Some (.-e datom))
        None)

      (Datascript_runtime.Data_value.Lookup_ref attr-name _)
      (if-some [eid (entid unfiltered entity-ref)]
        (if-some
          [_datom
           (first
            (-search
             database
             (search-pattern
              (Some eid)
              (Some (keyword attr-name))
              None
              None)))]
          (Some eid)
          None)
        None)

      Datascript_runtime.Data_value.Current_tx
      None

      (Datascript_runtime.Data_value.Temp_id _)
      None

      (Datascript_runtime.Data_value.Auto_tempid _)
      None)))

(defn  database-view-entid
  [ database
    entity-ref]
  (match database
    (DatabaseView unfiltered)
    (entid unfiltered entity-ref)
    (FilteredDatabaseView filtered)
    (filtered-entid filtered entity-ref)))

(defn  database-view-unfiltered-db
  [ database]
  (match database
    (DatabaseView unfiltered)
    unfiltered
    (FilteredDatabaseView filtered)
    (:unfiltered-db filtered)))

(defn  database-view-ref?
  [ database  attr]
  (ref? (database-view-unfiltered-db database) attr))

(defn  database-view-multival?
  [ database  attr]
  (multival? (database-view-unfiltered-db database) attr))

(defn  database-view-component?
  [ database  attr]
  (component? (database-view-unfiltered-db database) attr))

(signature datascript.db/database-view-search
  :fn<database-view;option<int>;option<keyword>;option<Datascript_runtime.Data_value.t>;option<int>;option<seq<Datom>>>)

(defn  database-view-search
  [ database
    entity
    attr
    value
    tx]
  (match database
    (DatabaseView unfiltered)
    (-search unfiltered (search-pattern entity attr value tx))
    (FilteredDatabaseView filtered)
    (if-some
      [datoms (-search filtered (search-pattern entity attr value tx))]
      (if (empty? datoms)
        None
        (Some datoms))
      None)))

(defn  database-view-search-vector
  [ database
    entity
    attr
    value
    tx]
  (match database
    (DatabaseView unfiltered)
    (search-vector unfiltered entity attr value tx)
    (FilteredDatabaseView _)
    (if-some
      [datoms
       (database-view-search database entity attr value tx)]
      (vec datoms)
      [])))

(defn  database-view-numeric-eid-exists?
  [ database  eid]
  (match database
    (DatabaseView unfiltered)
    (numeric-eid-exists? unfiltered eid)
    (FilteredDatabaseView filtered)
    (some?
     (first
      (-search
       filtered
       (search-pattern (Some eid) None None None))))))

(defn  database-view-identity-hash
  [ database]
  (match database
    (DatabaseView unfiltered)
    (:identity-hash unfiltered)
    (FilteredDatabaseView filtered)
    (:identity-hash filtered)))

(defn database-view-identical?
  [left right]
  (match (tuple left right)
    (tuple (DatabaseView left) (DatabaseView right))
    (identical? left right)
    (tuple
     (FilteredDatabaseView left)
     (FilteredDatabaseView right))
    (identical? left right)
    _
    false))

(signature datascript.db/database-view-reduce-eavt-slice
  [result]
  :fn<database-view;int;keyword;option<Datascript_runtime.Data_value.t>;fn<result;Datom;result>;result;result>)

(defn database-view-reduce-eavt-slice
  [ database
    entity
    attr
    value
   f
   initial]
  (match database
    (DatabaseView unfiltered)
    (reduce-eavt-slice unfiltered entity attr value f initial)
    (FilteredDatabaseView filtered)
    (reduce
     f
     initial
     (-search
      filtered
      (search-pattern (Some entity) (Some attr) value None)))))

(defn  numeric-eid-exists?
  [ db  eid]
  (= eid
     (-> (-seek-datoms
          db :eavt
          (Some (Datascript_runtime.Data_value.Int eid))
          nil nil nil)
         first
         :e)))

(signature datascript.db/entid-strict
  :fn<datascript.db/DB;Datascript_runtime.Data_value.entity_ref;int>)

(defn entid-strict
  [^datascript.db/DB db
   entity-ref]
  (if-some [eid (entid db entity-ref)]
    eid
    (match entity-ref
      (Datascript_runtime.Data_value.Lookup_ref attr-name value)
      (raise
       (Invalid_argument
        (str
         "Nothing found for entity id ["
         (keyword attr-name)
         " "
         (Datascript_runtime.Data_value.to_edn_string value)
         "]")))
      _
      (raise
       (Invalid_argument
        "Nothing found for entity reference")))))

(defn  entid-some
  [ db
    entity-ref]
  (if-some [entity-ref entity-ref]
    (Some (entid-strict db entity-ref))
    None))

(signature datascript.db/empty-entity-ids
  :vector<int>)

(defn data-value-tuple-items
  [ value]
  (if-some [items (Datascript_runtime.Data_value.tuple_items value)]
    items
    (if-some [items
              (Datascript_runtime.Data_value.sequential_items value)]
      (mapv
       (fn [item]
         (if (Datascript_runtime.Data_value.is_nil item)
           None
           (Some item)))
       items)
      (raise (Invalid_argument
              "Expected a DataScript tuple value")))))

(def empty-entity-ids
  [])

(signature datascript.db/resolve-tuple-refs
  :fn<datascript.db/DB;keyword;Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>)

(defn resolve-tuple-refs
  [db
   tuple-attr
   value]
  (let [value
        (Datascript_runtime.Data_value.tuple_of_vector
         (data-value-tuple-items value))
        attrs (schema-tuple-attrs db tuple-attr)
        ref-attrs
        (Rrbvec.of_list
         (Lg_runtime.Lg_set.String_set.elements
          (-attrs-by db :db.type/ref)))
        entity-refs
        (Datascript_runtime.Data_value.tuple_entity_refs
         attrs ref-attrs value)
        eids
        (loop [idx 0
               eids empty-entity-ids]
          (if (< idx (count entity-refs))
            (recur
             (inc idx)
             (conj eids
                   (entid-strict db (Rrbvec.nth entity-refs idx))))
            eids))]
    (Datascript_runtime.Data_value.resolve_tuple_refs
     attrs ref-attrs eids value)))

;;;;;;;;;; Transacting

(defn validate-datom [ db datom]
  (when (and (datom-added datom)
             (is-attr? db (.-a datom) :db/unique))
    (when-some
      [found
       (not-empty
        (-datoms
         db :avet
         (Some (Datascript_runtime.Data_value.Keyword (.-a datom)))
         (Some (.-v datom))
         nil nil))]
      (util/raise "Cannot add " datom " because of unique constraint: " found
                  {:error :transact/unique
                   :attribute (.-a datom)
                   :datom datom}))))

(defn- current-tx [report]
  (inc (:max-tx (:db-before report))))

(defn- next-eid [db]
  (inc (:max-eid db)))

(defn-  tempid? [ eid]
  (neg? eid))

(defn- new-eid? [db eid]
  (and (> eid (:max-eid db))
       (< eid tx0))) ;; tx0 is max eid

(defn- advance-max-eid
  [db eid]
  (cond-> db
    (new-eid? db eid)
    (assoc :max-eid eid)))

(defn  schema-ident-value
  [ value]
  (match value
    (Datascript_runtime.Data_value.Keyword ident)
    (keyword ident)
    _ (raise (Invalid_argument
              "Schema :db/ident must be a keyword"))))

(defn remove-schema [db datom]
  (let [schema        (effective-schema (:schema db))
        schema-idents (:schema-idents db)
        schema-drafts (:schema-drafts db)
        eid           (.-e datom)
        attr          (.-a datom)]
    (if (= attr :db/ident)
      (let [ident (schema-ident-value (.-v datom))]
        (if-some [entry (get schema ident)]
          (assoc db
                 :schema (Some (dissoc schema ident))
                 :schema-idents (dissoc schema-idents eid)
                 :schema-drafts
                 (assoc schema-drafts eid (dissoc entry :db/ident)))
          (util/raise "Schema with attribute " ident " does not exist"
                      {:error :retract/schema
                       :attribute ident})))
      (if-some [ident (get schema-idents eid)]
        (if-some [entry (get schema ident)]
          (assoc db :schema
                 (Some (assoc schema ident (dissoc entry attr))))
          (util/raise "Schema with attribute " ident " does not exist"
                      {:error :retract/schema
                       :attribute ident}))
        (if-some [draft (get schema-drafts eid)]
          (assoc db :schema-drafts
                 (assoc schema-drafts eid (dissoc draft attr)))
          (util/raise "Schema with entity id " eid " does not exist"
                      {:error :retract/schema
                       :entity-id eid
                       :attribute attr}))))))

(defn get-schema
  [db]
  (effective-schema (:schema db)))

(defn update-schema [db datom]
  (let [schema        (effective-schema (:schema db))
        schema-idents (:schema-idents db)
        schema-drafts (:schema-drafts db)
        eid           (.-e datom)
        attr          (.-a datom)
        value         (.-v datom)]
    (if (= attr :db/ident)
      (let [ident (schema-ident-value value)
            draft (or (get schema-drafts eid) empty-schema-entry)
            entry (assoc draft :db/ident value)]
        (assoc db
               :schema (Some (assoc schema ident entry))
               :schema-idents (assoc schema-idents eid ident)
               :schema-drafts (dissoc schema-drafts eid)))
      (if-some [ident (get schema-idents eid)]
        (let [entry (or (get schema ident) empty-schema-entry)]
          (assoc db :schema
                 (Some (assoc schema ident (assoc entry attr value)))))
        (let [draft (or (get schema-drafts eid) empty-schema-entry)]
          (assoc db :schema-drafts
                 (assoc schema-drafts eid (assoc draft attr value))))))))

(signature datascript.db/update-rschema
  :fn<datascript.db/DB;datascript.db/DB>)

(defn update-rschema [db]
  (assoc db :rschema
         (rschema (merge-schema implicit-schema (get-schema db)))))

;; In context of `with-datom` we can use faster comparators which
;; do not check for nil (~10-15% performance gain in `transact`)

(defn with-datom [db datom]
  (validate-datom db datom)
  (let [attr (datom-attr datom)
        indexing? (Lg_runtime.Core_set.String_set.mem
                   attr
                   (-attrs-by db :db/index))
        schema? (Lg_runtime.Core_set.String_set.mem attr ds/schema-attr?)]
    (if (datom-added datom)
      (cond-> db
        true      (update :eavt set/conj datom cmp-datoms-eavt-quick)
        true      (update :aevt set/conj datom cmp-datoms-aevt-quick)
        indexing? (update :avet set/conj datom cmp-datoms-avet-quick)
        true      (advance-max-eid (.-e datom))
        true      (assoc :identity-hash (fresh-db-identity))
        true      (assoc :hash (atom 0))
        schema?   (-> (update-schema datom)
                      update-rschema))
      (if-some [removing (fsearch db (.-e datom) attr (.-v datom) nil)]
        (cond-> db
          true      (update :eavt set/disj removing cmp-datoms-eavt-quick)
          true      (update :aevt set/disj removing cmp-datoms-aevt-quick)
          indexing? (update :avet set/disj removing cmp-datoms-avet-quick)
          true      (assoc :identity-hash (fresh-db-identity))
          true      (assoc :hash (atom 0))
          schema?   (-> (remove-schema datom) update-rschema))
        db))))

(defn  schema-tuple-arity
  [ db  tuple-attr]
  (if-some [entry (get (effective-schema (:schema db)) tuple-attr)]
    (if-some [value (get entry :db/tupleAttrs)]
      (match value
        (Datascript_runtime.Data_value.Vector attrs) (List.length attrs)
        (Datascript_runtime.Data_value.List attrs) (List.length attrs)
        _ (raise (Invalid_argument
                  "Schema tuple attrs must be a vector or list")))
      (raise (Invalid_argument
              "Schema tuple attribute has no :db/tupleAttrs")))
    (raise (Invalid_argument
            "Unknown schema tuple attribute"))))

(signature datascript.db/empty-queued-tuples
  :fn<unit;map<keyword;vector<option<Datascript_runtime.Data_value.t>>>>)

(defn empty-queued-tuples []
  {})

(signature datascript.db/queue-tuple
  :fn<map<keyword;vector<option<Datascript_runtime.Data_value.t>>>;keyword;int;datascript.db/DB;int;option<Datascript_runtime.Data_value.t>;map<keyword;vector<option<Datascript_runtime.Data_value.t>>>>)
(defn- queue-tuple
  [queue
   tuple-attr
   idx
   db
   eid
   value]
  (let [tuple-value
        (if-some [queued (get queue tuple-attr)]
          queued
          (if-some [existing (fsearch db eid tuple-attr nil nil)]
            (data-value-tuple-items (.-v existing))
            (vec (repeat (schema-tuple-arity db tuple-attr) None))))
        tuple-value' (assoc tuple-value idx value)]
    (assoc queue tuple-attr tuple-value')))

(defn- queue-tuples
  [queue
   tuples
   db
   eid
   value]
  (reduce-kv
   (fn [queue tuple-attr idx]
     (queue-tuple queue tuple-attr idx db eid value))
   queue
   tuples))

(defn-  transact-report [ report datom]
  (let [db      (:db-after report)
        a       (.-a datom)
        report'
        (assoc
         (assoc report :db-after (with-datom db datom))
         :tx-data
         (conj (.-tx-data report) datom))]
    (if (tuple-source? db a)
      (let [e      (.-e datom)
            v      (if (datom-added datom) (.-v datom) nil)
            queue  (or (-> report' :queued-tuples (get e))
                       (empty-queued-tuples))
            tuples
            (if-some [tuples (get (-attr-tuples db) a)]
              tuples
              (raise
               (Invalid_argument
                "Tuple source attribute has no tuple targets")))
            queue' (queue-tuples queue tuples db e v)]
        (assoc
         report'
         :queued-tuples
         (assoc (.-queued-tuples report') e queue')))
      report')))

(defn- resolve-upserts
  "Returns a tuple of the remaining entity attributes and resolved upserts.
   Upsert attributes that resolve to existing entities
  are removed from entity, rest are kept in entity for insertion. No validation is performed.

   upserts :: {:name  {\"Ivan\"  1}
               :email {\"ivan@\" 2}
               :alias {\"abc\"   3
                       \"def\"   4}}}"
  [db
   entity]
  (if-some [idents (not-empty (-attrs-by db :db.unique/identity))]
    (let [resolve
          (fn [a v]
            (if (ref? db a)
              (let [entity-ref (data-value-entity-ref v)]
                (if (temp-entity-ref? entity-ref)
                  None
                  (if-some [eid (entid db entity-ref)]
                    (if-some
                      [datom
                       (fsearch
                        db nil a
                        (Some (Datascript_runtime.Data_value.Ref eid))
                        nil)]
                      (Some (.-e datom))
                      None)
                    None)))
              (let [resolved
                    (if (tuple? db a)
                      (resolve-tuple-refs db a v)
                      v)]
                (if-some
                  [datom
                   (fsearch db nil a (Some resolved) nil)]
                  (Some (.-e datom))
                  None))))
          split
          (fn [a vs]
            (reduce
             (fn
               [acc v]
               (let [insert (tuple-get acc 0)
                     upsert (tuple-get acc 1)]
                 (if-some [e (resolve a v)]
                   (tuple insert (assoc upsert v e))
                   (tuple (conj insert v) upsert))))
             (tuple [] (empty-value-eids)) vs))]
      (reduce-kv
       (fn
         [acc a v]
         (let [entity' (tuple-get acc 0)
               upserts (tuple-get acc 1)]
           (validate-attr a entity)
           (validate-val v entity)
           (cond
             (not (contains? idents a))
             (tuple (assoc entity' a v) upserts)

             (multi-value? db a v)
             (let [values
                   (if-some
                     [values
                      (Datascript_runtime.Data_value.sequential_items v)]
                     values
                     (if-some
                       [values
                        (Datascript_runtime.Data_value.set_items v)]
                       values
                       (raise
                        (Invalid_argument
                         "Multi-valued attribute requires a collection"))))
                   result (split a values)
                   insert (tuple-get result 0)
                   upsert (tuple-get result 1)]
               (tuple
                (cond-> entity'
                  (not (empty? insert))
                  (assoc
                   a
                   (Datascript_runtime.Data_value.Vector
                    (Rrbvec.to_list insert))))
                (cond-> upserts
                  (not (empty? upsert)) (assoc a upsert))))

             :else
             (if-some [e (resolve a v)]
               (tuple entity' (assoc upserts a {v e}))
               (tuple (assoc entity' a v) upserts)))))
       (tuple (empty-entity-map) (empty-upsert-map))
       entity))
    (tuple entity (empty-upsert-map))))

(signature datascript.db/conflicting-upsert-message
  :fn<int;keyword;Datascript_runtime.Data_value.t;int;string>)

(signature datascript.db/validate-upsert-resolution-with-current-tx
  :fn<map<keyword;Datascript_runtime.Data_value.t>;option<tuple<int;keyword;Datascript_runtime.Data_value.t>>;option<int>;option<int>>)

(signature datascript.db/validate-upsert-resolution
  :fn<map<keyword;Datascript_runtime.Data_value.t>;option<tuple<int;keyword;Datascript_runtime.Data_value.t>>;option<int>>)

(signature datascript.db/validate-upserts
  :fn<map<keyword;Datascript_runtime.Data_value.t>;map<keyword;map<Datascript_runtime.Data_value.t;int>>;option<int>>)

(defn- conflicting-upsert-message
  [ upsert-id
    attr
    value
    explicit-eid]
  (str
   "Conflicting upsert: ["
   attr
   " "
   (Datascript_runtime.Data_value.to_edn_string value)
   "] resolves to "
   (Stdlib.string_of_int upsert-id)
   ", but entity already has :db/id "
   (Stdlib.string_of_int explicit-eid)))

(defn-  validate-upsert-resolution-with-current-tx
  [ entity
    resolution
    current-tx]
  (if-some [resolved resolution]
    (let [upsert-id (tuple-get resolved 0)
          attr      (tuple-get resolved 1)
          value     (tuple-get resolved 2)
          explicit-eid
          (if-some [eid-value (get entity :db/id)]
            (if-some
              [entity-ref
               (Datascript_runtime.Data_value.entity_ref_value eid-value)]
              (match entity-ref
                (Datascript_runtime.Data_value.Entity_id eid)
                (if (tempid? eid) None (Some eid))
                _ None)
              (match eid-value
                (Datascript_runtime.Data_value.Keyword ident)
                (if (= ident ":db/current-tx") current-tx None)
                _ None))
            None)]
      (if-some [eid explicit-eid]
        (when (not= upsert-id eid)
          (Stdlib.invalid_arg
           (conflicting-upsert-message upsert-id attr value eid)))
        nil)
      (Some upsert-id))
    None))

(defn  validate-upsert-resolution
  [ entity
    resolution]
  (validate-upsert-resolution-with-current-tx entity resolution None))

(defn validate-upserts
  "Throws if not all upserts point to the same entity.
   Returns single eid that all upserts point to, or null."
  [ entity
    upserts]
  (validate-upsert-resolution entity (collect-upsert-resolution upserts)))

(defn- validate-upserts-for-report
  [report
   entity
   upserts]
  (let [resolution (collect-upsert-resolution upserts)
        result
        (validate-upsert-resolution-with-current-tx
         entity resolution (Some (current-tx report)))]
    (if-some [resolved resolution]
      (if-some [id-value (get entity :db/id)]
        (if-some
          [entity-ref
           (Datascript_runtime.Data_value.entity_ref_value id-value)]
          (let [explicit-eid
                (match entity-ref
                  (Datascript_runtime.Data_value.Ident _)
                  (entid (:db-after report) entity-ref)
                  (Datascript_runtime.Data_value.Lookup_ref _ _)
                  (entid (:db-after report) entity-ref)
                  _ None)]
            (if-some [eid explicit-eid]
              (let [upsert-id (tuple-get resolved 0)]
                (when (not= upsert-id eid)
                  (Stdlib.invalid_arg
                   (conflicting-upsert-message
                    upsert-id
                    (tuple-get resolved 1)
                    (tuple-get resolved 2)
                    eid))))
              nil))
          nil)
        nil)
      nil)
    result))

(signature datascript.db/check-schema-update
  :fn<datascript.db/DB;map<keyword;Datascript_runtime.Data_value.t>;unit>)
(defn check-schema-update
  [_db entity]
  (when (ds/schema-entity? entity)
    (when-some [ident (get entity :db/ident)]
      (when (ds/is-system-keyword? ident)
        (util/raise
         "Using namespace 'db' for attribute identifiers is not allowed"
         {:error :transact/schema
          :entity entity})))
    (when
      (or
       (contains? entity :db/cardinality)
       (contains? entity :db/valueType))
      (when-not (ds/schema? entity)
        (util/raise
         "Incomplete schema transaction attributes, expected :db/ident, :db/cardinality"
         {:error :transact/schema
          :entity entity})))))

(defn singleton-data-value
  [value]
  [value])

(defn data-value-lookup-ref?
  [db
   values]
  (and
   (= (count values) 2)
   (if-some [first-value (first values)]
     (if-some [attr (Datascript_runtime.Data_value.keyword_value first-value)]
       (is-attr? db (keyword attr) :db.unique/identity)
       false)
     false)))

(defn- maybe-wrap-multival
  [db
   attr
   value]
  (if (not (or (reverse-ref? attr)
               (multival? db attr)))
    (singleton-data-value value)
    (if-some [values
              (Datascript_runtime.Data_value.sequential_items value)]
      (if (data-value-lookup-ref? db values)
        (singleton-data-value value)
        values)
      (if-some [values (Datascript_runtime.Data_value.set_items value)]
        values
        (singleton-data-value value)))))

(defn-
  data-value-entity-ref-with-error
  [ value  error-message]
  (if-some [entity-ref
            (Datascript_runtime.Data_value.entity_ref_value value)]
    entity-ref
    (if-some
      [lookup-ref
       (Datascript_runtime.Data_value.lookup_ref_value value)]
      (Datascript_runtime.Data_value.Lookup_ref
       (tuple-get lookup-ref 0)
       (tuple-get lookup-ref 1))
      (match value
        (Datascript_runtime.Data_value.String tempid)
        (Datascript_runtime.Data_value.Temp_id tempid)
        (Datascript_runtime.Data_value.Keyword ident)
        (if (= ident ":db/current-tx")
          (Datascript_runtime.Data_value.Current_tx)
          (Datascript_runtime.Data_value.Ident ident))
        _
        (raise (Invalid_argument error-message))))))

(defn  data-value-entity-ref
  [ value]
  (data-value-entity-ref-with-error
   value
   "Expected number or lookup ref for entity id"))

(defn  data-value-entity-form
  [ value]
  (if-some [entries
            (Datascript_runtime.Data_value.keyword_map_entries value)]
    (Some
     (reduce
      (fn [entity entry]
        (let [attr (keyword (tuple-get entry 0))]
          (record tx-entity-form
            (values
             (assoc
              (:values entity)
              attr
              (tuple-get entry 1)))
            (attrs (conj (:attrs entity) attr)))))
      (ordered-tx-entity (empty-entity-map) [])
      entries))
    None))

(signature datascript.db/tx-entity-form-assoc
  :fn<tx-entity-form;keyword;Datascript_runtime.Data_value.t;tx-entity-form>)

(defn tx-entity-form-assoc
  [ entity
    attr
    value]
  (record tx-entity-form
    (values (assoc (:values entity) attr value))
    (attrs
     (if (contains? (:values entity) attr)
       (:attrs entity)
       (conj (:attrs entity) attr)))))

(defn-  tx-entity-form-value
  [ entity]
  (Datascript_runtime.Data_value.map_of_keyword_entries
   (mapv
    (fn [attr]
      (if-some [value (get (:values entity) attr)]
        (tuple (str attr) value)
        (raise
         (Invalid_argument
          "Transaction entity attribute is missing from its value map"))))
    (:attrs entity))))

(defn-  assoc-auto-tempid-ref-value
  [ value
    multiple?

   prepare]
  (if multiple?
    (if-some
      [items (Datascript_runtime.Data_value.sequential_items value)]
      (Datascript_runtime.Data_value.vector_of_vector
       (mapv prepare items))
      (if-some [items (Datascript_runtime.Data_value.set_items value)]
        (Datascript_runtime.Data_value.set_of_vector
         (mapv prepare items))
        (prepare value)))
    (prepare value)))

(defn- assoc-auto-tempid-entity-value
  [db
   value]
  (if-some [source (data-value-entity-form value)]
    (let [entity
          (if (contains? (:values source) :db/id)
            source
            (tx-entity-form-assoc
             source
             :db/id
             (Datascript_runtime.Data_value.Ref_to
              (auto-tempid))))
          prepare
          (fn [item]
            (assoc-auto-tempid-entity-value db item))
          prepared
          (reduce
           (fn [result attr]
             (if (= attr :db/id)
               result
               (if-some [attr-value (get (:values result) attr)]
                 (let [reverse? (reverse-ref? attr)
                       straight-attr
                       (if reverse? (reverse-ref attr) attr)]
                   (if (or reverse? (ref? db straight-attr))
                     (tx-entity-form-assoc
                      result
                      attr
                      (assoc-auto-tempid-ref-value
                       attr-value
                       (if reverse?
                         (some?
                          (Datascript_runtime.Data_value.sequential_items
                           attr-value))
                         (multi-value? db straight-attr attr-value))
                       prepare))
                     result))
                 result)))
           entity
           (:attrs entity))]
      (tx-entity-form-value prepared))
    value))

(defn-  assoc-auto-tempid-entity-form
  [ db
    entity]
  (if-some
    [prepared
     (data-value-entity-form
      (assoc-auto-tempid-entity-value
       db
       (tx-entity-form-value entity)))]
    prepared
    (raise
     (Invalid_argument
      "Prepared transaction entity must remain a keyword map"))))

(defn-  assoc-auto-tempid-entry
  [ db  entry]
  (match entry
    (TxEntity entity)
    (TxEntity (assoc-auto-tempid-entity-form db entity))

    (TxInstallFunction entity callback)
    (let [prepared
          (assoc-auto-tempid-entity-form
           db
           (ordered-tx-entity entity (vec (keys entity))))]
      (TxInstallFunction (:values prepared) callback))

    (TxAdd entity-ref attr value transaction)
    (if (ref? db attr)
      (TxAdd
       entity-ref
       attr
       (assoc-auto-tempid-ref-value
        value
        (multi-value? db attr value)
        (fn [item]
          (assoc-auto-tempid-entity-value db item)))
       transaction)
      entry)

    _ entry))

(defn assoc-auto-tempids
  [db
   entries]
  (mapv
   (fn [entry]
     (assoc-auto-tempid-entry db entry))
   entries))

(defn empty-tx-entries []
  [])

(signature datascript.db/explode-attribute
  :fn<datascript.db/DB;Datascript_runtime.Data_value.entity_ref;vector<tx-entry>;keyword;Datascript_runtime.Data_value.t;vector<tx-entry>>)
(defn explode-attribute
  [db
   entity-ref
   entries
   attr
   value]
  (if (= attr :db/id)
    entries
    (let [reverse?   (reverse-ref? attr)
          straight-a (if reverse? (reverse-ref attr) attr)]
      (validate-attr attr (tuple entity-ref attr value))
      (when (and reverse? (not (ref? db straight-a)))
        (raise
         (Invalid_argument
          (str
           "Bad attribute " attr
           ": reverse attribute name requires "
           "{:db/valueType :db.type/ref} in schema"))))
      (reduce
       (fn [entries item]
         (conj
          entries
          (if-some
            [nested
             (if (ref? db straight-a)
               (data-value-entity-form item)
               None)]
            (let [nested
                  (tx-entity-form-assoc
                   nested
                   (reverse-ref attr)
                   (Datascript_runtime.Data_value.Ref_to entity-ref))]
              (tx-entity-ordered
               (:values nested)
               (:attrs nested)))
            (if reverse?
              (tx-add
               (data-value-entity-ref item)
               straight-a
               (Datascript_runtime.Data_value.Ref_to entity-ref))
              (tx-add entity-ref straight-a item)))))
       entries
       (maybe-wrap-multival db attr value)))))

(signature datascript.db/explode-pass
  :fn<datascript.db/DB;Datascript_runtime.Data_value.entity_ref;map<keyword;Datascript_runtime.Data_value.t>;vector<keyword>;bool;vector<tx-entry>;vector<tx-entry>>)
(defn explode-pass
  [db
   entity-ref
   entity
   attrs
   tuple-pass?
   entries]
  (reduce
   (fn [entries attr]
     (if-some [value (get entity attr)]
       (if (= tuple-pass? (tuple? db attr))
         (explode-attribute db entity-ref entries attr value)
         entries)
       entries))
   entries
   attrs))

(defn- explode
  [db
   entity
   attrs]
  (if-some [id-value (get entity :db/id)]
    (let [entity-ref
          (data-value-entity-ref-with-error
           id-value
           "Expected number, string or lookup ref for :db/id")
          entries (explode-pass db entity-ref entity attrs false
                                (empty-tx-entries))]
      (explode-pass db entity-ref entity attrs true entries))
    (raise (Invalid_argument
            "Transaction entity requires :db/id"))))

(defn-  transact-add
  [ report
    entity-ref
    attr
    value
    transaction]
  (validate-attr attr (tuple entity-ref attr value))
  (validate-val value (tuple entity-ref attr value))
  (let [transaction (if-some [transaction transaction]
                      transaction
                      (current-tx report))
        db          (:db-after report)
        eid         (entid-strict db entity-ref)
        value       (if (ref? db attr)
                      (Datascript_runtime.Data_value.Ref
                       (entid-strict db (data-value-entity-ref value)))
                      value)
        new-datom   (datom eid attr value transaction)
        multival?   (multival? db attr)
        old-datom  (if multival?
                           (fsearch db eid attr value nil)
                           (fsearch db eid attr nil nil))]
    (cond
      (nil? old-datom)
      (transact-report report new-datom)

      (= (.-v old-datom) value)
      report

      :else
      (-> report
          (transact-report
           (datom eid attr (.-v old-datom) transaction false))
          (transact-report new-datom)))))

(defn-  transact-retract-datom
  [ report d]
  (let [tx (current-tx report)]
    (transact-report report (datom (.-e d) (.-a d) (.-v d) tx false))))

(defn-  component-retraction
  [ db datom]
  (if (component? db (.-a datom))
    (if-some [eid (Datascript_runtime.Data_value.ref_value
                   (.-v datom))]
      (Some (tx-retract-entity-id eid))
      (raise
       (Invalid_argument
        "Component attribute value must be a resolved reference")))
    None))

(defn-  retract-components
  [ db  datoms]
  (Rrbvec.of_list
   (List.filter_map
    (fn [datom]
      (component-retraction db datom))
    (Rrbvec.to_list datoms))))

(signature datascript.db/queued-tuple-value
  :fn<vector<option<Datascript_runtime.Data_value.t>>;option<Datascript_runtime.Data_value.t>>)

(defn queued-tuple-value
  [ values]
  (if (every?
       (fn [value]
         (match value
           None true
           (Some _) false))
       values)
    None
    (Some (Datascript_runtime.Data_value.tuple_of_vector values))))

(defn flush-tuples [report]
  (let [db (:db-after report)]
    (reduce-kv
     (fn [entities eid tuples+values]
       (reduce-kv
        (fn [entities tuple-attr values]
          (let [value   (queued-tuple-value values)
                current (if-some [datom (fsearch db eid tuple-attr nil nil)]
                          (Some (.-v datom))
                          None)
                entity-ref (Datascript_runtime.Data_value.Entity_id eid)]
            (cond
              (= value current) entities
              (nil? value)
              (conj entities
                    (tx-set-tuple entity-ref tuple-attr None))
              :else
              (if-some [tuple-value value]
                (conj entities
                      (tx-set-tuple
                       entity-ref tuple-attr (Some tuple-value)))
                entities))))
        entities
        tuples+values))
     (empty-tx-entries)
     (:queued-tuples report))))

(defn  entity-ref-key
  [ entity-ref]
  (Datascript_runtime.Data_value.Ref_to entity-ref))

(defn-  direct-tuple-error
  [ operation
    entity-ref
    attr
    value]
  (str
   "Can’t modify tuple attrs directly: ["
   operation
   " "
   (Datascript_runtime.Data_value.to_edn_string
    (entity-ref-key entity-ref))
   " "
   attr
   " "
   (Datascript_runtime.Data_value.to_edn_string value)
   "]"))

(defn  current-tx-ref?
  [ entity-ref]
  (match entity-ref
    Datascript_runtime.Data_value.Current_tx true
    (Datascript_runtime.Data_value.Temp_id tempid)
    (or
     (= tempid "datomic.tx")
     (= tempid "datascript.tx"))
    (Datascript_runtime.Data_value.Ident ident)
    (= ident ":db/current-tx")
    _ false))

(defn allocate-tx-entity
  [report
   entity-ref]
  (let [key (entity-ref-key entity-ref)]
    (if-some [eid (get (.-tempids report) key)]
      (tuple report (Datascript_runtime.Data_value.Entity_id eid))
      (let [eid (next-eid (.-db-after report))
            tempids (assoc (.-tempids report) key eid)
            db-after (advance-max-eid (.-db-after report) eid)
            report (TxReport.
                    (.-db-before report)
                    db-after
                    (.-tx-data report)
                    tempids
                    (.-tx-meta report)
                    (.-queued-tuples report)
                    (.-value-tempids report)
                    (.-used-tempid-eids report))]
        (tuple report (Datascript_runtime.Data_value.Entity_id eid))))))

(defn resolve-tx-entity
  [report
   entity-ref]
  (if (current-tx-ref? entity-ref)
    (let [txid (current-tx report)]
      (tuple
       (assoc report :tempids
              (assoc
               (:tempids report)
               (entity-ref-key entity-ref)
               txid))
       (Datascript_runtime.Data_value.Entity_id txid)))
    (match entity-ref
      (Datascript_runtime.Data_value.Entity_id eid)
      (if (pos? eid)
        (tuple (update report :db-after advance-max-eid eid)
               entity-ref)
        (allocate-tx-entity report entity-ref))
      (Datascript_runtime.Data_value.Temp_id _)
      (allocate-tx-entity report entity-ref)
      (Datascript_runtime.Data_value.Auto_tempid _)
      (allocate-tx-entity report entity-ref)
      (Datascript_runtime.Data_value.Ident _)
      (tuple report
             (Datascript_runtime.Data_value.Entity_id
              (entid-strict (:db-after report) entity-ref)))
      (Datascript_runtime.Data_value.Lookup_ref _ _)
      (tuple report
             (Datascript_runtime.Data_value.Entity_id
              (entid-strict (:db-after report) entity-ref)))
      Datascript_runtime.Data_value.Current_tx
      (raise (Invalid_argument "Unreachable current transaction reference")))))

(defn  existing-tx-eid
  [ report
    entity-ref]
  (if (current-tx-ref? entity-ref)
    (Some (current-tx report))
    (match entity-ref
      (Datascript_runtime.Data_value.Entity_id eid)
      (if (pos? eid) (Some eid)
          (get (:tempids report) (entity-ref-key entity-ref)))
      (Datascript_runtime.Data_value.Temp_id _)
      (get (:tempids report) (entity-ref-key entity-ref))
      (Datascript_runtime.Data_value.Auto_tempid _)
      (get (:tempids report) (entity-ref-key entity-ref))
      (Datascript_runtime.Data_value.Ident _)
      (entid (:db-after report) entity-ref)
      (Datascript_runtime.Data_value.Lookup_ref _ _)
      (entid (:db-after report) entity-ref)
      Datascript_runtime.Data_value.Current_tx
      (Some (current-tx report)))))

(defn  temp-entity-ref?
  [ entity-ref]
  (if (current-tx-ref? entity-ref)
    false
    (match entity-ref
      (Datascript_runtime.Data_value.Entity_id eid) (not (pos? eid))
      (Datascript_runtime.Data_value.Temp_id _) true
      (Datascript_runtime.Data_value.Auto_tempid _) true
      _ false)))

(defn reject-tempid-operation
  [ entity-ref]
  (when (temp-entity-ref? entity-ref)
    (raise
     (Invalid_argument
      "Tempids are allowed in :db/add only"))))

(defn  data-value-description
  [ value]
  (Datascript_runtime.Data_value.to_edn_string value))

(defn  optional-data-value-description
  [ value]
  (if-some [value value]
    (data-value-description value)
    "nil"))

(defn reject-tempid-cas-operation
  [ operation
    entity-ref
    attr
    old-value
    new-value]
  (when (temp-entity-ref? entity-ref)
    (raise
     (Invalid_argument
      (str
       "Can't use tempid in '["
       operation
       " "
       (data-value-description
        (Datascript_runtime.Data_value.Ref_to entity-ref))
       " "
       attr
       " "
       (optional-data-value-description old-value)
       " "
       (data-value-description new-value)
       "]'. Tempids are allowed in :db/add only")))))

(defn  mark-entity-used
  [ report
    resolved-ref]
  (let [eid (entid-strict (:db-after report) resolved-ref)]
    (assoc
     report
     :used-tempid-eids
     (assoc (:used-tempid-eids report) eid true))))

(defn  resolve-tx-value
  [ report
    attr
    value]
  (let [db (:db-after report)]
    (if (ref? db attr)
      (let [entity-ref (data-value-entity-ref value)
            key (entity-ref-key entity-ref)
            result (resolve-tx-entity report entity-ref)
            report (tuple-get result 0)
            resolved-ref (tuple-get result 1)
            eid (entid-strict (:db-after report) resolved-ref)]
        (tuple
         (if (temp-entity-ref? entity-ref)
           (assoc report :value-tempids
                  (assoc
                   (:value-tempids report)
                   eid
                   key))
           report)
         (Datascript_runtime.Data_value.Ref eid)))
      (tuple report
             (if (tuple? db attr)
               (resolve-tuple-refs db attr value)
               value)))))

(defn direct-tuple-match
  [report
   entity-ref
   tuple-attr
   value]
  (let [attrs (schema-tuple-attrs (:db-after report) tuple-attr)
        source-items (data-value-tuple-items value)]
    (if (not= (count attrs) (count source-items))
      (tuple report false)
      (let [entity-result (resolve-tx-entity report entity-ref)
        report (tuple-get entity-result 0)
        resolved-ref (tuple-get entity-result 1)
        report (mark-entity-used report resolved-ref)
        db (:db-after report)
        eid (entid-strict db resolved-ref)
        value (resolve-tuple-refs db tuple-attr value)
        items (data-value-tuple-items value)
        matches?
        (loop [index 0]
          (if (< index (count attrs))
            (if-some [item (nth items index)]
              (if-some
                [datom
                 (fsearch db eid
                          (keyword (nth attrs index))
                          nil nil)]
                (if (= item (.-v datom))
                  (recur (inc index))
                  false)
                false)
              false)
            true))]
        (tuple report matches?)))))

(defn transact-add-entry
  [report
   entity-ref
   attr
   value
   transaction]
  (let [temp-entity? (temp-entity-ref? entity-ref)
        entity-result (resolve-tx-entity report entity-ref)
        report (tuple-get entity-result 0)
        entity-ref (tuple-get entity-result 1)
        report (mark-entity-used report entity-ref)
        value-result (resolve-tx-value report attr value)
        report (tuple-get value-result 0)
        value (tuple-get value-result 1)
        existing (if (is-attr? (:db-after report) attr
                               :db.unique/identity)
                   (fsearch (:db-after report) nil attr value nil)
                   None)
        entity-ref (if temp-entity?
                     (if-some [datom existing]
                       (Datascript_runtime.Data_value.Entity_id
                        (.-e datom))
                       entity-ref)
                     entity-ref)]
    (transact-add report entity-ref attr value transaction)))

(defn transact-retract-entry
  [report
   entity-ref
   attr
   value]
  (if-some [eid (existing-tx-eid report entity-ref)]
    (if-some [value value]
      (let [value-result (resolve-tx-value report attr value)
            report (tuple-get value-result 0)
            value (tuple-get value-result 1)]
        (if-some
          [datom
           (fsearch
            (:db-after report)
            (Some eid) (Some attr) (Some value) nil)]
          (transact-retract-datom report datom)
          report))
      (reduce transact-retract-datom report
              (-search
               (:db-after report)
               (search-pattern (Some eid) (Some attr) nil nil))))
    report))

(defn  incoming-reference-datoms
  [db eid]
  (reduce
   (fn [datoms attr]
     (into
      datoms
      (search-vector
       db nil (Some attr)
       (Some (Datascript_runtime.Data_value.Ref eid))
       nil)))
   []
   (Lg_runtime.Lg_set.String_set.elements
    (-attrs-by db :db.type/ref))))

(defn  data-values-description
  [ values]
  (str
   "("
   (loop [index 0
          description ""]
     (if (< index (count values))
       (let [separator (if (= index 0) "" " ")]
         (recur
          (inc index)
          (str
           description
           separator
           (data-value-description (nth values index)))))
       description))
   ")"))

(defn  datom-values-description
  [ datoms]
  (data-values-description
   (mapv
    (fn [datom]
      (.-v datom))
    datoms)))

(defn transact-cas-entry
  [report
   entity-ref
   attr
   old-value
   new-value]
  (if-some [eid (existing-tx-eid report entity-ref)]
    (let [db (:db-after report)
          resolve-value
          (fn [value]
            (if (ref? db attr)
              (Datascript_runtime.Data_value.Ref
               (entid-strict db (data-value-entity-ref value)))
              (if (tuple? db attr)
                (resolve-tuple-refs db attr value)
                value)))
          old-value
          (if-some [value old-value]
            (Some (resolve-value value))
            None)
          new-value (resolve-value new-value)
          datoms
          (search-vector db (Some eid) (Some attr) nil nil)
          current-description
          (if (multival? db attr)
            (datom-values-description datoms)
            (if-some [datom (first datoms)]
              (data-value-description (.-v datom))
              "nil"))
          matches?
          (if (multival? db attr)
            (if-some [expected old-value]
              (loop [index 0]
                (if (< index (count datoms))
                  (if (= (.-v (nth datoms index)) expected)
                    true
                    (recur (inc index)))
                  false))
              false)
            (if-some [expected old-value]
              (if-some [datom (first datoms)]
                (= (.-v datom) expected)
                false)
              (empty? datoms)))]
      (if matches?
        (transact-add
         report
         (Datascript_runtime.Data_value.Entity_id eid)
         attr new-value None)
        (raise
         (Invalid_argument
          (str
           ":db.fn/cas failed on datom ["
           eid
           " "
           attr
           " "
           current-description
           "], expected "
           (optional-data-value-description old-value))))))
    (raise (Invalid_argument
            "Compare-and-set entity does not exist"))))

(signature datascript.db/tempid-values-description
  :fn<map<int;Datascript_runtime.Data_value.t>;string>)

(signature datascript.db/remove-auto-tempids
  :fn<map<Datascript_runtime.Data_value.t;int>;map<Datascript_runtime.Data_value.t;int>>)

(defn tempid-values-description
  [ tempids]
  (data-values-description (vec (vals tempids))))

(defn-  remove-auto-tempids
  [ tempids]
  (reduce-kv
   (fn
     [retained key eid]
     (if-some
       [entity-ref
        (Datascript_runtime.Data_value.entity_ref_value key)]
       (match entity-ref
         (Datascript_runtime.Data_value.Auto_tempid _) retained
         _ (assoc retained key eid))
       (assoc retained key eid)))
   {}
   tempids))

(defn check-value-tempids
  [report]
  (let [unused-tempids
        (reduce-kv
         (fn
           [unused eid _used]
           (dissoc unused eid))
         (:value-tempids report)
         (:used-tempid-eids report))]
    (if (empty? unused-tempids)
      report
      (raise
       (Invalid_argument
        (str
         "Tempids used only as value in transaction: "
         (tempid-values-description unused-tempids)))))))

(defn- finish-transaction-db
  [database]
  (assoc
   (assoc database :max-tx (inc (:max-tx database)))
   :identity-hash
   (fresh-db-identity)))

(defn finish-transaction
  [report]
  (let [report (check-value-tempids report)
        current (current-tx report)]
    (assoc
     (assoc
      report
      :tempids
      (assoc
       (remove-auto-tempids (:tempids report))
       (entity-ref-key
        (Datascript_runtime.Data_value.Current_tx))
       current))
     :db-after
     (finish-transaction-db (:db-after report)))))

(signature datascript.db/push-tx-continuation
  :fn<vector<tuple<vector<tx-entry>;int>>;vector<tx-entry>;int;vector<tuple<vector<tx-entry>;int>>>)
(defn- push-tx-continuation
  [continuations
   entries
   entry-index]
  (into [(tuple entries entry-index)] continuations))

(defn-
  continue-with-generated
  [report
   generated
   entries
   next-index
   continuations]
  (if (= 0 (count generated))
    (tuple report entries next-index continuations)
    (tuple
     report
     generated
     0
     (push-tx-continuation continuations entries next-index))))

(defn-
  retract-attribute-next
  [report
   entity-ref
   attr
   entries
   next-index
   continuations]
  (if-some [eid (existing-tx-eid report entity-ref)]
    (let [db (:db-after report)
          datoms (search-vector db (Some eid) (Some attr) nil nil)
          generated (retract-components db datoms)
          report (reduce transact-retract-datom report datoms)]
      (continue-with-generated
       report generated entries next-index continuations))
    (tuple report entries next-index continuations)))

(defn-
  retract-entity-next
  [report
   entity-ref
   entries
   next-index
   continuations]
  (if-some [eid (existing-tx-eid report entity-ref)]
    (let [db (:db-after report)
          entity-datoms (search-vector db (Some eid) nil nil nil)
          reference-datoms (incoming-reference-datoms db eid)
          generated (retract-components db entity-datoms)
          report
          (reduce transact-retract-datom report entity-datoms)
          report
          (reduce transact-retract-datom report reference-datoms)]
      (continue-with-generated
       report generated entries next-index continuations))
    (tuple report entries next-index continuations)))

(defn- mark-upserted-eid-used
  [report eid]
  (assoc
   report
   :used-tempid-eids
   (assoc (.-used-tempid-eids report) eid true)))

(defn- bind-tempid-upsert
  [report
   entity-ref
   upserted-eid]
  (let [key (entity-ref-key entity-ref)]
    (if-some [existing (get (:tempids report) key)]
      (if (= existing upserted-eid)
        (mark-upserted-eid-used report upserted-eid)
        (raise (Invalid_argument
                "Conflicting upsert tempid resolution")))
      (mark-upserted-eid-used
       (assoc report :tempids
              (assoc (:tempids report) key upserted-eid))
       upserted-eid))))

(defn- bind-upserted-entity
  [report
   entity-ref
   upserted-eid]
  (match entity-ref
    (Datascript_runtime.Data_value.Entity_id eid)
    (if (pos? eid)
      (if (= eid upserted-eid)
        report
        (raise (Invalid_argument "Conflicting upsert entity id")))
      (bind-tempid-upsert report entity-ref upserted-eid))
    (Datascript_runtime.Data_value.Temp_id _)
    (bind-tempid-upsert report entity-ref upserted-eid)
    (Datascript_runtime.Data_value.Auto_tempid _)
    (bind-tempid-upsert report entity-ref upserted-eid)
    Datascript_runtime.Data_value.Current_tx
    (if (= (current-tx report) upserted-eid)
      report
      (raise (Invalid_argument "Conflicting current transaction upsert")))
    (Datascript_runtime.Data_value.Ident _)
    (if (= (entid-strict (:db-after report) entity-ref)
           upserted-eid)
      report
      (raise (Invalid_argument "Conflicting ident upsert")))
    (Datascript_runtime.Data_value.Lookup_ref _ _)
    (if (= (entid-strict (:db-after report) entity-ref)
           upserted-eid)
      report
      (raise (Invalid_argument "Conflicting lookup-ref upsert")))))

(defn- tempid-upsert-retry-needed?
  [report
   entity-ref
   upserted-eid]
  (if-some [existing
            (get (:tempids report) (entity-ref-key entity-ref))]
    (not= existing upserted-eid)
    false))

(defn- upsert-retry-needed?
  [report
   entity-ref
   upserted-eid]
  (match entity-ref
    (Datascript_runtime.Data_value.Entity_id eid)
    (if (pos? eid)
      false
      (tempid-upsert-retry-needed?
       report entity-ref upserted-eid))
    (Datascript_runtime.Data_value.Temp_id _)
    (tempid-upsert-retry-needed?
     report entity-ref upserted-eid)
    (Datascript_runtime.Data_value.Auto_tempid _)
    (tempid-upsert-retry-needed?
     report entity-ref upserted-eid)
    _ false))

(defn-
  temp-entity-ref-for-eid
  [ report  eid]
  (reduce-kv
   (fn
     [found key resolved-eid]
     (match found
       (Some _) found
       None
       (if (= resolved-eid eid)
         (if-some
           [entity-ref
            (Datascript_runtime.Data_value.entity_ref_value key)]
           (if (temp-entity-ref? entity-ref)
             (Some entity-ref)
             None)
           None)
         None)))
   None
   (:tempids report)))

(defn-  unique-upsert-eid
  [ report
    attr
    value]
  (let [db (:db-after report)]
    (if (is-attr? db attr :db.unique/identity)
      (let [resolved
            (if (ref? db attr)
              (let [entity-ref (data-value-entity-ref value)]
                (if-some [eid (existing-tx-eid report entity-ref)]
                  (Some (Datascript_runtime.Data_value.Ref eid))
                  None))
              (Some
               (if (tuple? db attr)
                 (resolve-tuple-refs db attr value)
                 value)))]
        (if-some [resolved resolved]
          (if-some [datom (fsearch db nil attr resolved nil)]
            (Some (.-e datom))
            None)
          None))
      None)))

(defn-
  tuple-upsert-retry
  [ report
    entity-ref
    attr
    value]
  (if-some [upserted-eid (unique-upsert-eid report attr value)]
    (if-some [current-eid (existing-tx-eid report entity-ref)]
      (if (= current-eid upserted-eid)
        None
        (if-some [tempid (temp-entity-ref-for-eid report current-eid)]
          (Some (tuple tempid upserted-eid))
          None))
      None)
    None))

(signature datascript.db/assoc-entity-id
  :fn<map<keyword;Datascript_runtime.Data_value.t>;int;map<keyword;Datascript_runtime.Data_value.t>>)
(defn- assoc-entity-id
  [entity eid]
  (assoc
   entity
   :db/id
   (Datascript_runtime.Data_value.Ref_to
    (Datascript_runtime.Data_value.Entity_id eid))))

(defn-
  expand-entity-next
  [report
   entity
   attrs
   entries
   next-index
   continuations]
  (if-some [id-value (get entity :db/id)]
    (let [entity-ref
          (data-value-entity-ref-with-error
           id-value
           "Expected number, string or lookup ref for :db/id")]
      (tuple
       report
       (explode (:db-after report) entity attrs)
       0
       (push-tx-continuation
        continuations entries next-index)))
    (raise
     (Invalid_argument "Transaction entity requires :db/id"))))

(type-variant tx-step-result
  (TxStep
   :tuple<datascript.db/TxReport;vector<tx-entry>;int;vector<tuple<vector<tx-entry>;int>>>)
  (TxRestart :Datascript_runtime.Data_value.entity_ref :int))

(type-variant tx-run-result
  (TxFinished :datascript.db/TxReport)
  (TxRetry
   :datascript.db/TxReport
   :Datascript_runtime.Data_value.entity_ref
   :int))

(defn transact-tx-data-loop
  [report
   entries
   entry-index
   continuations]
  (if (< entry-index (count entries))
      (let [entry (nth entries entry-index)
            next-index (inc entry-index)
            step
            (match entry
              TxNil
              (TxStep
               (tuple report entries next-index continuations))

              TxFlushTuples
              (let [queued (flush-tuples report)
                    report (assoc report :queued-tuples {})]
                (TxStep
                 (tuple
                  report
                  queued
                  0
                  (push-tx-continuation
                   continuations entries next-index))))

              (TxCall callback)
              (TxStep
               (continue-with-generated
                report
                (assoc-auto-tempids
                 (:db-after report)
                 (callback (:db-after report)))
                entries
                next-index
                continuations))

              (TxInstallFunction entity callback)
              (if-some [ident-value (get entity :db/ident)]
                (match ident-value
                  (Datascript_runtime.Data_value.Keyword ident)
                  (let [db (.-db-after report)
                        functions (.-tx-functions db)
                        report
                        (assoc
                         report
                         :db-after
                         (assoc
                          db
                          :tx-functions
                          (assoc functions ident callback)))]
                    (TxStep
                     (continue-with-generated
                      report
                      (assoc-auto-tempids
                       db
                       [(tx-entity entity)])
                      entries
                      next-index
                      continuations)))
                  _
                  (raise
                   (Invalid_argument
                    "Transaction function :db/ident must be a keyword")))
                (raise
                 (Invalid_argument
                  "Transaction function entity requires :db/ident")))

              (TxInvokeFunction ident arguments)
              (let [db (.-db-after report)]
                (if-some [callback (get (.-tx-functions db) ident)]
                  (if (= 1 (count arguments))
                    (TxStep
                     (continue-with-generated
                      report
                      (callback db (nth arguments 0))
                      entries
                      next-index
                      continuations))
                    (raise
                     (Invalid_argument
                      (str
                       "Transaction function " ident
                       " expects one argument"))))
                  (if-some
                    [_entity
                     (entid
                      db
                      (Datascript_runtime.Data_value.Ident ident))]
                    (raise
                     (Invalid_argument
                      (str
                       "Entity " ident
                       " expected to have :db/fn attribute with fn? value")))
                    (raise
                     (Invalid_argument
                      (str
                       "Can’t find entity for transaction fn "
                       ident))))))

              (TxInvalid message)
              (raise (Invalid_argument message))

              (TxEntity entity-form)
              (let [entity (:values entity-form)
                    attrs (:attrs entity-form)
                    _ (check-schema-update (.-db-after report) entity)
                    db (:db-after report)
                    upsert-result (resolve-upserts db entity)
                    entity (tuple-get upsert-result 0)
                    upserts (tuple-get upsert-result 1)
                    upserted-eid
                    (validate-upserts-for-report report entity upserts)]
                (if-some [eid upserted-eid]
                  (if-some [id-value (get entity :db/id)]
                    (let [entity-ref
                          (data-value-entity-ref-with-error
                           id-value
                           "Expected number, string or lookup ref for :db/id")]
                      (if (upsert-retry-needed?
                           report entity-ref eid)
                        (TxRestart entity-ref eid)
                        (let [report
                              (bind-upserted-entity
                               report entity-ref eid)
                              entity (assoc-entity-id entity eid)]
                          (TxStep
                           (expand-entity-next
                            report entity attrs entries next-index
                            continuations)))))
                    (TxStep
                     (expand-entity-next
                      report
                      (assoc-entity-id entity eid)
                      attrs entries next-index continuations)))
                  (TxStep
                   (expand-entity-next
                    report entity attrs entries next-index
                    continuations))))

              (TxAdd entity-ref attr value transaction)
              (if (tuple? (:db-after report) attr)
                (if-some [upserted-eid
                          (unique-upsert-eid report attr value)]
                  (if (upsert-retry-needed?
                       report entity-ref upserted-eid)
                    (TxRestart entity-ref upserted-eid)
                    (let [report
                          (bind-upserted-entity
                           report entity-ref upserted-eid)
                          result
                          (direct-tuple-match
                           report entity-ref attr value)
                          report (tuple-get result 0)]
                      (if (tuple-get result 1)
                        (TxStep
                         (tuple report entries next-index continuations))
                        (raise
                         (Invalid_argument
                          (direct-tuple-error
                           :db/add entity-ref attr value))))))
                  (let [result
                        (direct-tuple-match
                         report entity-ref attr value)
                        report (tuple-get result 0)]
                    (if (tuple-get result 1)
                      (TxStep
                       (tuple report entries next-index continuations))
                      (raise
                       (Invalid_argument
                        (direct-tuple-error
                         :db/add entity-ref attr value))))))
                (if-some [upserted-eid
                          (unique-upsert-eid report attr value)]
                  (if (upsert-retry-needed?
                       report entity-ref upserted-eid)
                    (TxRestart entity-ref upserted-eid)
                    (let [report
                          (bind-upserted-entity
                           report entity-ref upserted-eid)]
                      (TxStep
                       (tuple
                        (transact-add-entry report entity-ref attr value
                                            transaction)
                        entries
                        next-index
                        continuations))))
                  (TxStep
                   (tuple
                    (transact-add-entry report entity-ref attr value
                                        transaction)
                    entries
                    next-index
                    continuations))))

              (TxRetract entity-ref attr value)
              (let [_ (reject-tempid-operation entity-ref)]
                (if (tuple? (:db-after report) attr)
                  (if-some [tuple-value value]
                    (let [result
                          (direct-tuple-match
                           report entity-ref attr tuple-value)
                          report (tuple-get result 0)]
                      (if (tuple-get result 1)
                        (TxStep
                         (tuple report entries next-index continuations))
                        (raise
                         (Invalid_argument
                          (direct-tuple-error
                           :db/retract entity-ref attr tuple-value)))))
                    (raise
                     (Invalid_argument
                      "Cannot modify tuple attributes directly")))
                  (TxStep
                   (tuple
                    (transact-retract-entry report entity-ref attr value)
                    entries
                    next-index
                    continuations))))

              (TxRetractAttribute entity-ref attr)
              (let [_ (reject-tempid-operation entity-ref)]
                (TxStep
                 (retract-attribute-next
                  report entity-ref attr entries next-index continuations)))

              (TxRetractEntity entity-ref)
              (let [_ (reject-tempid-operation entity-ref)]
                (TxStep
                 (retract-entity-next
                  report entity-ref entries next-index continuations)))

              (TxSetTuple entity-ref attr value)
              (if-some [tuple-value value]
                (if-some
                  [retry
                   (tuple-upsert-retry
                    report entity-ref attr tuple-value)]
                  (TxRestart
                   (tuple-get retry 0)
                   (tuple-get retry 1))
                  (TxStep
                   (tuple
                    (transact-add-entry
                     report entity-ref attr tuple-value None)
                    entries
                    next-index
                    continuations)))
                (TxStep
                 (tuple
                  (transact-retract-entry
                   report entity-ref attr None)
                  entries
                  next-index
                  continuations)))

              (TxCas operation entity-ref attr old-value new-value)
              (let [_ (reject-tempid-cas-operation
                       operation
                       entity-ref
                       attr
                       old-value
                       new-value)]
                (TxStep
                 (tuple
                  (transact-cas-entry
                   report entity-ref attr old-value new-value)
                  entries
                  next-index
                  continuations))))]
        (match step
          (TxStep next)
          (transact-tx-data-loop
           (tuple-get next 0)
           (tuple-get next 1)
           (tuple-get next 2)
           (tuple-get next 3))
          (TxRestart entity-ref eid)
          (TxRetry report entity-ref eid)))
      (if-some [continuation (first continuations)]
        (transact-tx-data-loop
         report
         (tuple-get continuation 0)
         (tuple-get continuation 1)
         (subvec continuations 1))
        (if (empty? (:queued-tuples report))
          (TxFinished (finish-transaction report))
          (let [queued (flush-tuples report)
                report (assoc report :queued-tuples {})]
            (transact-tx-data-loop
             report queued 0 continuations))))))

(defn transact-tx-data-run
  [initial-report
   initial-entries]
  (transact-tx-data-loop initial-report initial-entries 0 []))

(defn- temp-entity-ref-description
  [entity-ref]
  (match entity-ref
    (Datascript_runtime.Data_value.Entity_id eid) (Stdlib.string_of_int eid)
    (Datascript_runtime.Data_value.Temp_id tempid) tempid
    (Datascript_runtime.Data_value.Auto_tempid auto-tempid)
    (str
     "#datascript/AutoTempid ["
     (Stdlib.string_of_int auto-tempid)
     "]")
    _ (raise (Invalid_argument "Expected a transaction tempid"))))

(signature datascript.db/transact-tx-data-impl-with-upserts
  :fn<datascript.db/TxReport;vector<tx-entry>;map<Datascript_runtime.Data_value.t;int>;datascript.db/TxReport>)
(defn- transact-tx-data-impl-with-upserts
  [initial-report
   initial-entries
   upserted-tempids]
  (match
    (transact-tx-data-run initial-report initial-entries)
    (TxFinished report)
    report
    (TxRetry report entity-ref upserted-eid)
    (let [key (entity-ref-key entity-ref)]
      (if-some [existing (get upserted-tempids key)]
        (raise
         (Invalid_argument
          (str
           "Conflicting upsert: "
           (temp-entity-ref-description entity-ref)
           " resolves both to " upserted-eid
           " and " existing)))
        (transact-tx-data-impl-with-upserts
         (assoc
          initial-report
          :tempids
          (assoc (.-tempids report) key upserted-eid))
         initial-entries
         (assoc upserted-tempids key upserted-eid))))))

(defn transact-tx-data-impl
  [initial-report
   initial-entries]
  (transact-tx-data-impl-with-upserts
   initial-report
   initial-entries
   {}))

(defn transact-tx-data
  [report
   entries]
  (transact-tx-data-impl
   report
   (assoc-auto-tempids (:db-before report) entries)))
