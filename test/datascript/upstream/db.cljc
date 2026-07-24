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

(def ^:map<keyword;Datascript_runtime.Data_value.t> empty-schema-entry
  {})

(def ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> empty-schema
  {})

(def ^:map<int;keyword> empty-schema-idents
  {})

(def ^:map<int;map<keyword;Datascript_runtime.Data_value.t>> empty-schema-drafts
  {})

(defn ^:map<keyword;Datascript_runtime.Data_value.t> implicit-schema-entry []
  (assoc
   empty-schema-entry
   :db/unique
   (Datascript_runtime.Data_value.Keyword ":db.unique/identity")))

(def implicit-schema
  (assoc empty-schema
         :db/ident
         (implicit-schema-entry)))

(defn ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> merge-schema
  [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> base
   ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> overrides]
  (reduce-kv
   (fn [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> result
        ^:keyword attr
        ^:map<keyword;Datascript_runtime.Data_value.t> entry]
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

;; ----------------------------------------------------------------------------

(declare equiv-datom datom-hash datom-tx datom-added)

(defprotocol IDatom
  (datom-get-idx [datom] :int)
  (datom-set-idx [datom ^:int value] :unit))

(deftype Datom [^long e
                ^:keyword a
                ^:Datascript_runtime.Data_value.t v
                ^long tx
                ^:mutable ^int idx
                ^:mutable ^int cached-hash]
  IDatom
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

(defn datom-tx [^Datom d]
  (let [tx (.-tx d)]
    (if (pos? tx) tx (- tx))))

(defn ^:keyword datom-attr [^Datom datom]
  (.-a datom))

(defn datom-added [^Datom d]
  (pos? (.-tx d)))

(defn ^Datom datom
  ([^:int e
    ^:keyword a
    ^:Datascript_runtime.Data_value.t v]
   (Datom. e a v tx0 0 0))
  ([^:int e
    ^:keyword a
    ^:Datascript_runtime.Data_value.t v
    ^:int tx]
   (Datom. e a v tx 0 0))
  ([^:int e
    ^:keyword a
    ^:Datascript_runtime.Data_value.t v
    ^:int tx
    ^boolean added]
   (Datom. e a v (if added tx (- tx)) 0 0)))

(def ^:private attr-wildcard (keyword ""))
(def ^:private value-wildcard (Datascript_runtime.Data_value.Nil))

(defn ^Datom datom-bound
  [^:option<int> e
   ^:option<keyword> a
   ^:option<Datascript_runtime.Data_value.t> v
   ^:option<int> tx
   ^:int default-e
   ^:int default-tx]
  (datom
   (if-some [value e] value default-e)
   (if-some [value a] value attr-wildcard)
   (if-some [value v] value value-wildcard)
   (if-some [value tx] value default-tx)))

(defn ^:private equiv-datom [^Datom d ^Datom o]
  (and (== (.-e d) (.-e o))
       (= (.-a d) (.-a o))
       (Datascript_runtime.Data_value.equal (.-v d) (.-v o))))

(defn datom-vectors-equal?
  [^:vector<Datom> left ^:vector<Datom> right]
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
  ^long [^:keyword x ^:keyword y]
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

(defn ^number value-compare
  [^:Datascript_runtime.Data_value.t x
   ^:Datascript_runtime.Data_value.t y]
  (Datascript_runtime.Data_value.compare x y))

(defn value-cmp
  ^long [^:Datascript_runtime.Data_value.t x
         ^:Datascript_runtime.Data_value.t y]
  (if (Datascript_runtime.Data_value.is_nil x)
    0
    (if (Datascript_runtime.Data_value.is_nil y)
      0
      (value-compare x y))))

;; Slower cmp-* fns allows for datom fields to be nil.
;; Such datoms come from slice method where they are used as boundary markers.

(defn cmp-datoms-eavt ^long [^Datom d1, ^Datom d2]
  (combine-cmp
   (int-compare (.-e d1) (.-e d2))
   (cmp (.-a d1) (.-a d2))
   (value-cmp (.-v d1) (.-v d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

(defn cmp-datoms-aevt ^long [^Datom d1, ^Datom d2]
  (combine-cmp
   (cmp (.-a d1) (.-a d2))
   (int-compare (.-e d1) (.-e d2))
   (value-cmp (.-v d1) (.-v d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

(defn cmp-datoms-avet ^long [^Datom d1, ^Datom d2]
  (combine-cmp
   (cmp (.-a d1) (.-a d2))
   (value-cmp (.-v d1) (.-v d2))
   (int-compare (.-e d1) (.-e d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

;; fast versions without nil checks

(defn- cmp-attr-quick ^long [^:keyword a1 ^:keyword a2]
  (compare a1 a2))

(defn cmp-datoms-eav-quick ^long [^Datom d1, ^Datom d2]
  (combine-cmp
   (int-compare (.-e d1) (.-e d2))
   (cmp-attr-quick (.-a d1) (.-a d2))
   (value-compare (.-v d1) (.-v d2))))

(defn cmp-datoms-eavt-quick ^long [^Datom d1, ^Datom d2]
  (combine-cmp
   (int-compare (.-e d1) (.-e d2))
   (cmp-attr-quick (.-a d1) (.-a d2))
   (value-compare (.-v d1) (.-v d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

(defn cmp-datoms-aevt-quick ^long [^Datom d1, ^Datom d2]
  (combine-cmp
   (cmp-attr-quick (.-a d1) (.-a d2))
   (int-compare (.-e d1) (.-e d2))
   (value-compare (.-v d1) (.-v d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

(defn cmp-datoms-avet-quick ^long [^Datom d1, ^Datom d2]
  (combine-cmp
   (cmp-attr-quick (.-a d1) (.-a d2))
   (value-compare (.-v d1) (.-v d2))
   (int-compare (.-e d1) (.-e d2))
   (int-compare (datom-tx d1) (datom-tx d2))))

;; ----------------------------------------------------------------------------

(declare restore-db typed-index)

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
     `(when (nil? ~v)
        (let [at# ~at]
          (util/raise "Cannot store nil as a value at " at#
                      {:error :transact/syntax, :value nil, :context at#})))))

;;;;;;;;;; Searching

(defprotocol ISearch
  (-search
   [data
    ^:option<int> e
    ^:option<keyword> a
    ^:option<Datascript_runtime.Data_value.t> v
    ^:option<int> tx]
   :seq<Datom>))

(defprotocol IIndexAccess
  (-datoms
   [db
    ^:keyword index
    ^:option<Datascript_runtime.Data_value.t> c0
    ^:option<Datascript_runtime.Data_value.t> c1
    ^:option<Datascript_runtime.Data_value.t> c2
    ^:option<Datascript_runtime.Data_value.t> c3]
   :seq<Datom>)
  (-seek-datoms
   [db
    ^:keyword index
    ^:option<Datascript_runtime.Data_value.t> c0
    ^:option<Datascript_runtime.Data_value.t> c1
    ^:option<Datascript_runtime.Data_value.t> c2
    ^:option<Datascript_runtime.Data_value.t> c3]
   :seq<Datom>)
  (-rseek-datoms
   [db
    ^:keyword index
    ^:option<Datascript_runtime.Data_value.t> c0
    ^:option<Datascript_runtime.Data_value.t> c1
    ^:option<Datascript_runtime.Data_value.t> c2
    ^:option<Datascript_runtime.Data_value.t> c3]
   :seq<Datom>)
  (-index-range
   [db
    ^:keyword attr
    ^:option<Datascript_runtime.Data_value.t> start
    ^:option<Datascript_runtime.Data_value.t> end]
   :seq<Datom>))

(defn ^:option<keyword> index-component-keyword
  [^:option<Datascript_runtime.Data_value.t> component]
  (if-some [component component]
    (if-some [keyword-text
              (Datascript_runtime.Data_value.keyword_value component)]
      (Some (keyword keyword-text))
      (raise (Invalid_argument "Index attribute component must be a keyword")))
    None))

(defn ^:option<Datascript_runtime.Data_value.entity_ref> index-component-entity-ref
  [^:option<Datascript_runtime.Data_value.t> component]
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
   :map<keyword;map<keyword;Datascript_runtime.Data_value.t>>)
  (-attrs-by [db ^:keyword property] :set<keyword>)
  (-attr-tuples [db] :map<keyword;map<keyword;int>>)
  (-unfiltered-db [db] :datascript.db/DB))

;; ----------------------------------------------------------------------------

(defrecord DB [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
               ^:map<int;keyword> schema-idents
               ^:map<int;map<keyword;Datascript_runtime.Data_value.t>> schema-drafts
               ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> eavt
               ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> aevt
               ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> avet
               ^:int max-eid
               ^:int max-tx
               ^datascript.db/ReverseSchema rschema
               ^:ref<int> hash]
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

  ISearch
  (-search [db
            ^:option<int> e
            ^:option<keyword> a
            ^:option<Datascript_runtime.Data_value.t> v
            ^:option<int> tx]
           (let [eavt       (.-eavt db)
                 aevt       (.-aevt db)
                 avet       (.-avet db)
                 pred       (fn [^:Datascript_runtime.Data_value.t candidate]
                              (if-some [value v]
                                (= value candidate)
                                false))]
             (case-tree [e a (some? v) tx]
                        [(set/slice eavt (datom-bound e a v tx e0 tx0) (datom-bound e a v tx e0 tx0)) ;; e a v tx
                         (set/slice eavt (datom-bound e a v nil e0 tx0) (datom-bound e a v nil e0 txmax)) ;; e a v _
                         (->> (set/slice eavt (datom-bound e a nil nil e0 tx0) (datom-bound e a nil nil e0 txmax)) ;; e a _ tx
                              (->Eduction (filter (fn [^Datom d] (= tx (datom-tx d))))))
                         (set/slice eavt (datom-bound e a nil nil e0 tx0) (datom-bound e a nil nil e0 txmax)) ;; e a _ _
                         (->> (set/slice eavt (datom-bound e nil nil nil e0 tx0) (datom-bound e nil nil nil e0 txmax)) ;; e _ v tx
                              (->Eduction (filter (fn [^Datom d] (and (pred (.-v d))
                                                                      (= tx (datom-tx d)))))))
                         (->> (set/slice eavt (datom-bound e nil nil nil e0 tx0) (datom-bound e nil nil nil e0 txmax)) ;; e _ v _
                              (->Eduction (filter (fn [^Datom d] (pred (.-v d))))))
                         (->> (set/slice eavt (datom-bound e nil nil nil e0 tx0) (datom-bound e nil nil nil e0 txmax)) ;; e _ _ tx
                              (->Eduction (filter (fn [^Datom d] (= tx (datom-tx d))))))
                         (set/slice eavt (datom-bound e nil nil nil e0 tx0) (datom-bound e nil nil nil e0 txmax)) ;; e _ _ _
                         (if (if-some [attr a] (contains? (-attrs-by db :db/index) attr) false) ;; _ a v tx
                           (->> (set/slice avet (datom-bound nil a v nil e0 tx0) (datom-bound nil a v nil emax txmax))
                                (->Eduction (filter (fn [^Datom d] (= tx (datom-tx d))))))
                           (->> (set/slice aevt (datom-bound nil a nil nil e0 tx0) (datom-bound nil a nil nil emax txmax))
                                (->Eduction (filter (fn [^Datom d] (and (pred (.-v d))
                                                                        (= tx (datom-tx d))))))))
                         (if (if-some [attr a] (contains? (-attrs-by db :db/index) attr) false) ;; _ a v _
                           (set/slice avet (datom-bound nil a v nil e0 tx0) (datom-bound nil a v nil emax txmax))
                           (->> (set/slice aevt (datom-bound nil a nil nil e0 tx0) (datom-bound nil a nil nil emax txmax))
                                (->Eduction (filter (fn [^Datom d] (pred (.-v d)))))))
                         (->> (set/slice aevt (datom-bound nil a nil nil e0 tx0) (datom-bound nil a nil nil emax txmax)) ;; _ a _ tx
                              (->Eduction (filter (fn [^Datom d] (= tx (datom-tx d))))))
                         (set/slice aevt (datom-bound nil a nil nil e0 tx0) (datom-bound nil a nil nil emax txmax)) ;; _ a _ _
                         (filter (fn [^Datom d] (and (pred (.-v d))
                                                     (= tx (datom-tx d)))) (set/set-seq eavt))  ;; _ _ v tx
                         (filter (fn [^Datom d] (pred (.-v d))) (set/set-seq eavt))             ;; _ _ v
                         (filter (fn [^Datom d] (= tx (datom-tx d))) (set/set-seq eavt))        ;; _ _ _ tx
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

(defn validate-indexed
  [^datascript.db/DB db
   ^:keyword index
   ^:option<Datascript_runtime.Data_value.t> c0
   ^:option<Datascript_runtime.Data_value.t> c1
   ^:option<Datascript_runtime.Data_value.t> c2
   ^:option<Datascript_runtime.Data_value.t> c3]
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
  (-search [db e a v tx]
           (filter (.-pred db) (-search (.-unfiltered-db db) e a v tx)))

  IIndexAccess
  (-datoms [db index c0 c1 c2 c3]
           (filter (.-pred db) (-datoms (.-unfiltered-db db) index c0 c1 c2 c3)))

  (-seek-datoms [db index c0 c1 c2 c3]
                (filter (.-pred db) (-seek-datoms (.-unfiltered-db db) index c0 c1 c2 c3)))

  (-rseek-datoms [db index c0 c1 c2 c3]
                 (filter (.-pred db) (-rseek-datoms (.-unfiltered-db db) index c0 c1 c2 c3)))

  (-index-range [db attr start end]
                (filter (.-pred db) (-index-range (.-unfiltered-db db) attr start end))))

(defprotocol IFilteredView
  (-filtered? [db] :bool)
  (-filter-view
   [db ^:fn<datascript.db/DB;datascript.db/Datom;bool> pred]
   :datascript.db/FilteredDB))

(extend-type DB
  IFilteredView
  (-filtered? [_db] false)
  (-filter-view
    [db ^:fn<datascript.db/DB;datascript.db/Datom;bool> view-pred]
    (FilteredDB.
     db
     (fn [^datascript.db/Datom datom]
       (view-pred db datom))
     (atom 0))))

(extend-type FilteredDB
  IFilteredView
  (-filtered? [_db] true)
  (-filter-view
    [filtered-db
     ^:fn<datascript.db/DB;datascript.db/Datom;bool> view-pred]
    (let [original-pred (.-pred filtered-db)
          original-db (.-unfiltered-db filtered-db)]
      (FilteredDB.
       original-db
       (fn [^datascript.db/Datom datom]
         (and
          (original-pred datom)
          (view-pred original-db datom)))
       (atom 0)))))

(defn- ^:option<Datom> fsearch
  [^datascript.db/DB data
   ^:option<int> e
   ^:option<keyword> a
   ^:option<Datascript_runtime.Data_value.t> v
   ^:option<int> tx]
  (first (-search data e a v tx)))

(defn ^:option<Datom> search-ea
  [^datascript.db/DB data ^:int eid ^:keyword attr]
  (fsearch data (Some eid) (Some attr) None None))

(defn unfiltered-db ^datascript.db/DB [db]
  (-unfiltered-db db))

(defn db-equal? [^datascript.db/DB left ^datascript.db/DB right]
  (and
   (= (.-schema left) (.-schema right))
   (datom-vectors-equal?
    (vec (set/set-seq (.-eavt left)))
    (vec (set/set-seq (.-eavt right))))))

(defn ^:int datom-hash [^Datom datom]
  (Hashtbl.hash
   (tuple
    (.-e datom)
    (.-a datom)
    (Datascript_runtime.Data_value.hash (.-v datom)))))

(defn ^:Datascript_runtime.Data_value.t schema-entry-value
  [^:map<keyword;Datascript_runtime.Data_value.t> entry]
  (Datascript_runtime.Data_value.map_of_keyword_map entry))

(defn ^:int schema-hash
  [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
  (Datascript_runtime.Data_value.hash
   (Datascript_runtime.Data_value.map_of_keyword_map_with
    schema-entry-value
    schema)))

(defn ^:int db-hash [^datascript.db/DB database]
  (let [cached @(:hash database)]
    (if (zero? cached)
      (let [value
            (reduce
             (fn [^:int value ^Datom datom]
               (Hashtbl.hash (tuple value (datom-hash datom))))
             (schema-hash (:schema database))
             (set/set-seq (.-eavt database)))]
        (reset! (:hash database) value)
        value)
      cached)))

(defn ^:int db-count [^datascript.db/DB database]
  (count (.-eavt database)))

(defn ^:vector<Datom> filtered-db-datoms
  [^datascript.db/FilteredDB database]
  (vec (-datoms database :eavt nil nil nil nil)))

(defn filtered-db-equal?
  [^datascript.db/FilteredDB left
   ^datascript.db/FilteredDB right]
  (and
   (= (-schema left) (-schema right))
   (datom-vectors-equal?
    (filtered-db-datoms left)
    (filtered-db-datoms right))))

(defn ^:int filtered-db-hash
  [^datascript.db/FilteredDB database]
  (let [cached @(:hash database)]
    (if (zero? cached)
      (let [value
            (reduce
             (fn [^:int value ^Datom datom]
               (Hashtbl.hash (tuple value (datom-hash datom))))
             (schema-hash (-schema database))
             (filtered-db-datoms database))]
        (reset! (:hash database) value)
        value)
      cached)))

(defn ^:int filtered-db-count
  [^datascript.db/FilteredDB database]
  (count (filtered-db-datoms database)))

(extend-type DB
  IEquiv
  (-equiv [^datascript.db/DB db ^datascript.db/DB other]
    (db-equal? db other))

  IHash
  (-hash [^datascript.db/DB db] (db-hash db))

  ICounted
  (-count [^datascript.db/DB db] (db-count db)))

(extend-type FilteredDB
  IEquiv
  (-equiv
    [^datascript.db/FilteredDB db
     ^datascript.db/FilteredDB other]
    (filtered-db-equal? db other))

  IHash
  (-hash [^datascript.db/FilteredDB db] (filtered-db-hash db))

  ICounted
  (-count [^datascript.db/FilteredDB db] (filtered-db-count db)))

;; ----------------------------------------------------------------------------

(defn ^:vector<keyword> attr->properties
  [^:keyword k ^:Datascript_runtime.Data_value.t v]
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

(defn ^:map<keyword;map<keyword;int>> attr-tuples
  "e.g. :reg/semester => #{:reg/semester+course+student ...}"
  [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
   ^:map<keyword;set<keyword>> rschema]
  (reduce
   (fn [^:map<keyword;map<keyword;int>> m ^:keyword tuple-attr] ;; e.g. :reg/semester+course+student
     (let [^:vector<keyword> attrs
           (if-some
            [attrs
             (Datascript_runtime.Data_value.keyword_items
             (get
               (get schema tuple-attr empty-schema-entry)
               :db/tupleAttrs
               value-wildcard))]
             (mapv (fn [^:string attr] (keyword attr)) attrs)
             [])]
       (reduce-kv
        (fn [^:map<keyword;map<keyword;int>> m ^:int idx ^:keyword src-attr]
          (assoc
           m
           src-attr
           (assoc (get m src-attr {}) tuple-attr idx)))
        m
        attrs)))
   {}
   (:db.type/tuple rschema)))

(defn- rschema
  [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
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
                 (fn [^:map<keyword;set<keyword>> m
                      ^:keyword attr
                      ^:map<keyword;Datascript_runtime.Data_value.t> keys->values]
                   (reduce-kv
                    (fn [^:map<keyword;set<keyword>> m
                         ^:keyword key
                         ^:Datascript_runtime.Data_value.t value]
                      (reduce
                       (fn [^:map<keyword;set<keyword>> m ^:keyword prop]
                         (update
                          m prop
                          (fn [^:option<set<keyword>> attrs]
                            (if-some [attrs attrs]
                              (conj attrs attr)
                              #{attr}))))
                       m (attr->properties key value)))
                    (update
                     m
                     :db/ident
                     (fn [^:option<set<keyword>> attrs]
                       (if-some [attrs attrs]
                         (conj attrs attr)
                         #{attr})))
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

(defn- invalid-schema-value [^:keyword a ^:keyword k]
  (Stdlib.invalid_arg
   (str "Bad attribute specification for " a
        ": " k " has an invalid value")))

(defn- ^:option<keyword> schema-keyword
  [^:map<keyword;Datascript_runtime.Data_value.t> entry ^:keyword key]
  (if-some [value (get entry key)]
    (Datascript_runtime.Data_value.keyword_value value)
    None))

(defn- ^:option<bool> schema-bool
  [^:map<keyword;Datascript_runtime.Data_value.t> entry ^:keyword key]
  (if-some [value (get entry key)]
    (Datascript_runtime.Data_value.bool_value value)
    None))

(defn- validate-schema-keyword
  [^:keyword a
   ^:keyword k
   ^:map<keyword;Datascript_runtime.Data_value.t> entry
   ^:set<keyword> expected]
  (when-some [value (get entry k)]
    (if-some [keyword-value
              (Datascript_runtime.Data_value.keyword_value value)]
      (when-not (contains? expected (keyword keyword-value))
        (invalid-schema-value a k))
      (invalid-schema-value a k))))

(defn- validate-schema-bool
  [^:keyword a
   ^:keyword k
   ^:map<keyword;Datascript_runtime.Data_value.t> entry]
  (when-some [value (get entry k)]
    (when-not
      (some? (Datascript_runtime.Data_value.bool_value value))
      (invalid-schema-value a k))))

(defn- validate-tuple-schema
  [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
   ^:keyword a
   ^:map<keyword;Datascript_runtime.Data_value.t> entry]
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
           (str a " :db/tupleAttrs cannot be empty")))

        (doseq [attr-name attrs]
          (let [attr (keyword attr-name)
                dependency (get schema attr empty-schema-entry)]
            (when (contains? dependency :db/tupleAttrs)
              (Stdlib.invalid_arg
               (str a
                    " :db/tupleAttrs cannot depend on another tuple attribute: "
                    attr)))

            (when (= (schema-keyword dependency :db/cardinality)
                     (Some :db.cardinality/many))
              (Stdlib.invalid_arg
               (str a
                    " :db/tupleAttrs cannot depend on a many-valued attribute: "
                    attr))))))
      (Stdlib.invalid_arg
       (str a " :db/tupleAttrs must be a keyword vector or list")))))

(defn- validate-schema
  [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
  (reduce-kv
   (fn [^:unit _
        ^:keyword a
        ^:map<keyword;Datascript_runtime.Data_value.t> entry]
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
          (str "Bad attribute specification for " a
               ": :db/isComponent requires :db.type/ref"))))

     (when (= (schema-keyword entry :db/valueType)
              (Some :db.type/tuple))
       (when-not (contains? entry :db/tupleAttrs)
         (Stdlib.invalid_arg
          (str "Bad attribute specification for " a
               ": :db.type/tuple requires :db/tupleAttrs"))))

     (validate-tuple-schema schema a entry)
     (Stdlib.ignore 0))
   (Stdlib.ignore 0)
   schema))

(type-record database-options
  (storage :option<Datascript_runtime.Storage_backend.t>)
  (ref-type :Lg_runtime.Runtime_ref_type.t))

(defn ^database-options default-options []
  (record database-options
          (storage None)
          (ref-type (Lg_runtime.Runtime_ref_type.Weak))))

(defn ^database-options options-with-storage
  [^Datascript_runtime.Storage_backend.t storage]
  (record database-options
          (storage (Some storage))
          (ref-type (Lg_runtime.Runtime_ref_type.Weak))))

(defn ^database-options options-with-ref-type
  [^database-options opts ^:Lg_runtime.Runtime_ref_type.t ref-type]
  (record database-options
          (storage (.-storage opts))
          (ref-type ref-type)))

(defn ^:option<Datascript_runtime.Storage_backend.t> options-storage
  [^database-options opts]
  (.-storage opts))

(defn ^:Lg_runtime.Runtime_ref_type.t options-ref-type
  [^database-options opts]
  (.-ref-type opts))

(defn- empty-datom-set [comparator ^database-options opts]
  (set/with-ref-type
   (set/sorted-set-with-comparator
    comparator
    None)
   (options-ref-type opts)))

(defn ^datascript.db/DB empty-db
  [^:option<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>> maybe-schema
   ^database-options opts]
  (let [schema (if-some [schema maybe-schema] schema empty-schema)]
  (validate-schema schema)
  (record DB
          (schema schema)
          (schema-idents empty-schema-idents)
          (schema-drafts empty-schema-drafts)
          (eavt (empty-datom-set cmp-datoms-eavt opts))
          (aevt (empty-datom-set cmp-datoms-aevt opts))
          (avet (empty-datom-set cmp-datoms-avet opts))
          (max-eid e0)
          (max-tx tx0)
          (rschema (rschema (merge-schema implicit-schema schema)))
          (hash (atom 0)))))

(defn- init-max-eid [rschema eavt avet]
  (let [max     (fn [^:int current ^:option<int> candidate]
                  (if-some [candidate candidate]
                    (if (> candidate current) candidate current)
                    current))
        max-eid (some->
                 (set/rslice eavt
                             (datom (dec tx0) attr-wildcard value-wildcard txmax)
                             (datom e0 attr-wildcard value-wildcard tx0))
                 first :e)
        res     (max e0 max-eid)
        max-ref (fn [^:keyword attr]
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

(defn- ^:int init-max-tx [^:array<Datom> datoms]
  (loop [index 0
         result tx0]
    (if (< index (arrays/alength datoms))
      (recur
       (inc index)
       (max result (datom-tx (arrays/aget datoms index))))
      result)))

(defn- ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> datom-set-from-sorted-array
  [^:ordering-fn comparator
   ^:array<Datom> datoms
   ^:int length
   ^database-options opts]
  (set/from-sorted-array
   comparator
   datoms
   length
   None
   (options-ref-type opts)))

(defn ^datascript.db/DB init-db
  ([^:array<Datom> datoms
    ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
   (init-db datoms schema (default-options)))
  ([^:array<Datom> datoms
    ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
    ^database-options opts]
  (validate-schema schema)
  (let [rschema     (rschema (merge-schema implicit-schema schema))
        indexed     (:indexed-attrs rschema)
        arr         datoms
        _           (arrays/asort arr cmp-datoms-eavt-quick)
        eavt        (datom-set-from-sorted-array
                     cmp-datoms-eavt arr (arrays/alength arr) opts)
        _           (arrays/asort arr cmp-datoms-aevt-quick)
        aevt        (datom-set-from-sorted-array
                     cmp-datoms-aevt arr (arrays/alength arr) opts)
        avet-datoms (filter (fn [^Datom d]
                             (Lg_runtime.Core_set.String_set.mem
                              (datom-attr d)
                              indexed))
                           datoms)
        avet-arr    (to-array avet-datoms)
        _           (arrays/asort avet-arr cmp-datoms-avet-quick)
        avet        (datom-set-from-sorted-array
                     cmp-datoms-avet avet-arr (arrays/alength avet-arr) opts)
        max-eid     (init-max-eid rschema eavt avet)
        max-tx      (init-max-tx arr)]
    (DB.
     schema
     empty-schema-idents
     empty-schema-drafts
     eavt
     aevt
     avet
     max-eid
     max-tx
     rschema
     (atom 0)))))

(type-record db-snapshot
  (schema :map<keyword;map<keyword;Datascript_runtime.Data_value.t>>)
  (schema-idents :option<map<int;keyword>>)
  (schema-drafts :option<map<int;map<keyword;Datascript_runtime.Data_value.t>>>)
  (rschema :option<datascript.db/ReverseSchema>)
  (eavt :set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>>)
  (aevt :set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>>)
  (avet :set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>>)
  (max-eid :option<int>)
  (max-tx :option<int>))

(defn ^db-snapshot make-db-snapshot
  [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
   ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> eavt
   ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> aevt
   ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> avet
   ^:int max-eid
   ^:int max-tx]
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

(defn ^datascript.db/DB restore-db
  [^db-snapshot snapshot]
  (let [schema (:schema snapshot)]
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
       (rschema (merge-schema implicit-schema schema)))
     (atom 0))))

(defn ^datascript.db/DB restore-db-from-storage
  [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
   ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> eavt
   ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> aevt
   ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> avet
   ^:int max-eid
   ^:int max-tx]
  (restore-db
   (make-db-snapshot schema eavt aevt avet max-eid max-tx)))

(defn with-schema
  [^datascript.db/DB db
   ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
  (assoc db
         :schema        schema
         :schema-idents empty-schema-idents
         :schema-drafts empty-schema-drafts
         :rschema       (rschema (merge-schema implicit-schema schema))
         :hash          (atom 0)))

(defn- ^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> typed-index
  [^DB db index]
  (case index
    :eavt (.-eavt db)
    :aevt (.-aevt db)
    :avet (.-avet db)
    (util/raise "Unknown index " index)))

(defn- ^:vector<Datom> datom-slice-vector
  [^:set/btset<Datom;Datascript_runtime.Storage_backend.t;tuple<int;Datascript_runtime.Storage_value.t>> datom-set
   ^Datom from
   ^Datom to
   ^:fn<Datom;bool> include?]
  (set/set-slice-reduce-with
   datom-set
   from
   to
   (set/comparator datom-set)
   (fn [^:vector<Datom> datoms ^Datom datom]
     (if (include? datom)
       (conj datoms datom)
       datoms))
   []))

(defn ^:vector<Datom> search-vector
  [^DB db
   ^:option<int> e
   ^:option<keyword> a
   ^:option<Datascript_runtime.Data_value.t> v
   ^:option<int> tx]
  (match (tuple e a v tx)
    (tuple (Some entity) (Some attr) value None)
    (datom-slice-vector
     (.-eavt db)
     (datom-bound
      (Some entity) (Some attr) value None e0 tx0)
     (datom-bound
      (Some entity) (Some attr) value None e0 txmax)
     (fn [_datom] true))

    (tuple None (Some attr) (Some value) None)
    (if (contains? (-attrs-by db :db/index) attr)
      (datom-slice-vector
       (.-avet db)
       (datom-bound None (Some attr) (Some value) None e0 tx0)
       (datom-bound None (Some attr) (Some value) None emax txmax)
       (fn [_datom] true))
      (datom-slice-vector
       (.-aevt db)
       (datom-bound None (Some attr) None None e0 tx0)
       (datom-bound None (Some attr) None None emax txmax)
       (fn [^Datom datom]
         (Datascript_runtime.Data_value.equal (.-v datom) value))))

    _
    (vec (-search db e a v tx))))

;; ----------------------------------------------------------------------------

(declare entid-strict ref?)

(defn ^Datom resolve-datom
  [^DB db
   ^:option<Datascript_runtime.Data_value.entity_ref> e
   ^:option<keyword> a
   ^:option<Datascript_runtime.Data_value.t> v
   ^:option<Datascript_runtime.Data_value.entity_ref> tx
   ^:int default-e
   ^:int default-tx]
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
  [^DB db
   ^:keyword index
   ^:option<Datascript_runtime.Data_value.t> c0
   ^:option<Datascript_runtime.Data_value.t> c1
   ^:option<Datascript_runtime.Data_value.t> c2
   ^:option<Datascript_runtime.Data_value.t> c3
   ^:int default-e
   ^:int default-tx]
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

(defn find-datom [^DB db index c0 c1 c2 c3]
  (validate-indexed db index c0 c1 c2 c3)
  (let [set     (typed-index db index)
        cmp     (set/comparator set)
        from    (components->pattern db index c0 c1 c2 c3 e0 tx0)
        to      (components->pattern db index c0 c1 c2 c3 emax txmax)
        datom   (set/seek-first set from cmp)]
    (when (and (some? datom) (<= 0 (cmp to datom)))
      datom)))

;; ----------------------------------------------------------------------------

(defn ^:map<keyword;Datascript_runtime.Data_value.t> empty-entity-map []
  {})

(defn ^:map<Datascript_runtime.Data_value.t;int> empty-value-eids []
  {})

(defn ^:map<keyword;map<Datascript_runtime.Data_value.t;int>> empty-upsert-map []
  {})

(type-variant tx-entry
  (TxEntity :map<keyword;Datascript_runtime.Data_value.t>)
  (TxAdd :Datascript_runtime.Data_value.entity_ref
         :keyword
         :Datascript_runtime.Data_value.t
         :option<int>)
  (TxRetract :Datascript_runtime.Data_value.entity_ref
             :keyword
             :option<Datascript_runtime.Data_value.t>)
  (TxCas :Datascript_runtime.Data_value.entity_ref
         :keyword
         :option<Datascript_runtime.Data_value.t>
         :Datascript_runtime.Data_value.t)
  (TxRetractAttribute :Datascript_runtime.Data_value.entity_ref :keyword)
  (TxRetractEntity :Datascript_runtime.Data_value.entity_ref)
  (TxSetTuple :Datascript_runtime.Data_value.entity_ref
              :keyword
              :option<Datascript_runtime.Data_value.t>)
  TxFlushTuples)

(defn ^tx-entry tx-entity
  [^:map<keyword;Datascript_runtime.Data_value.t> entity]
  (TxEntity entity))

(defn ^tx-entry tx-add
  [^:Datascript_runtime.Data_value.entity_ref entity
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (TxAdd entity attr value None))

(defn ^tx-entry tx-retract-entity-id [^:int entity]
  (TxRetractEntity
   (Datascript_runtime.Data_value.Entity_id entity)))

(defn ^tx-entry tx-retract-attribute
  [^:Datascript_runtime.Data_value.entity_ref entity ^:keyword attr]
  (TxRetractAttribute entity attr))

(defn ^tx-entry tx-cas
  [^:Datascript_runtime.Data_value.entity_ref entity
   ^:keyword attr
   ^:option<Datascript_runtime.Data_value.t> old-value
   ^:Datascript_runtime.Data_value.t new-value]
  (TxCas entity attr old-value new-value))

(defn ^tx-entry datom->tx-entry [^Datom datom]
  (let [entity-ref
        (Datascript_runtime.Data_value.Entity_id (.-e datom))
        attr (datom-attr datom)
        value (.-v datom)]
    (if (datom-added datom)
      (TxAdd entity-ref attr value (Some (datom-tx datom)))
      (TxRetract entity-ref attr (Some value)))))

(defn ^tx-entry tx-add
  [^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (TxAdd entity-ref attr value None))

(defn ^:option<tuple<int;keyword;Datascript_runtime.Data_value.t>> add-upsert-resolution
  [^:option<tuple<int;keyword;Datascript_runtime.Data_value.t>> resolution
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value
   ^:int eid]
  (if-some [existing resolution]
    (let [existing-eid   (tuple-get existing 0)
          existing-attr  (tuple-get existing 1)
          existing-value (tuple-get existing 2)]
      (if (= existing-eid eid)
        resolution
        (util/raise "Conflicting upserts: attribute " existing-attr
                    " value " existing-value " resolves to " existing-eid
                    ", but attribute " attr " value " value
                    " resolves to " eid
                    {:error     :transact/upsert
                     :assertion (tuple existing-eid existing-attr existing-value)
                     :conflict  (tuple eid attr value)})))
    (Some (tuple eid attr value))))

(defn ^:option<tuple<int;keyword;Datascript_runtime.Data_value.t>> empty-upsert-resolution []
  None)

(defn ^:option<tuple<int;keyword;Datascript_runtime.Data_value.t>> add-value-eids
  [^:option<tuple<int;keyword;Datascript_runtime.Data_value.t>> resolution
   ^:keyword attr
   ^:map<Datascript_runtime.Data_value.t;int> value-eids]
  (reduce-kv
   (fn [^:option<tuple<int;keyword;Datascript_runtime.Data_value.t>> resolution
        ^:Datascript_runtime.Data_value.t value
        ^:int eid]
     (add-upsert-resolution resolution attr value eid))
   resolution
   value-eids))

(defn ^:option<tuple<int;keyword;Datascript_runtime.Data_value.t>> collect-upsert-resolution
  [^:map<keyword;map<Datascript_runtime.Data_value.t;int>> upserts]
  (reduce-kv
   (fn [^:option<tuple<int;keyword;Datascript_runtime.Data_value.t>> resolution
        ^:keyword attr
        ^:map<Datascript_runtime.Data_value.t;int> value-eids]
     (add-value-eids resolution attr value-eids))
   (empty-upsert-resolution)
   upserts))

(defrecord TxReport
  [^datascript.db/DB db-before
   ^datascript.db/DB db-after
   ^:vector<Datom> tx-data
   ^:map<Datascript_runtime.Data_value.t;int> tempids
   ^:map<keyword;Datascript_runtime.Data_value.t> tx-meta
   ^:map<int;map<keyword;vector<option<Datascript_runtime.Data_value.t>>>> queued-tuples
   ^:map<int;Datascript_runtime.Data_value.t> value-tempids])

(defn ^:int tx-data-count [^TxReport report]
  (count (.-tx-data report)))

(defn ^boolean is-attr?
  [^datascript.db/DB db ^:keyword attr ^:keyword property]
  (contains? (-attrs-by db property) attr))

(defn ^boolean multival?
  [^datascript.db/DB db ^:keyword attr]
  (is-attr? db attr :db.cardinality/many))

(defn ^boolean multi-value?
  [^datascript.db/DB db
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (and
   (is-attr? db attr :db.cardinality/many)
   (or
    (some? (Datascript_runtime.Data_value.sequential_items value))
    (some? (Datascript_runtime.Data_value.set_items value)))))

(defn ^boolean ref? [^datascript.db/DB db ^:keyword attr]
  (is-attr? db attr :db.type/ref))

(defn ^boolean component? [^datascript.db/DB db ^:keyword attr]
  (is-attr? db attr :db/isComponent))

(defn ^boolean tuple? [^datascript.db/DB db ^:keyword attr]
  (is-attr? db attr :db.type/tuple))

(defn ^boolean tuple-source? [^datascript.db/DB db ^:keyword attr]
  (contains? (-attr-tuples db) attr))

(defn ^boolean reverse-ref? [^:keyword attr]
  (= \_ (nth (name attr) 0)))

(defn ^:keyword reverse-ref [^:keyword attr]
  (if (reverse-ref? attr)
    (keyword (namespace attr) (subs (name attr) 1))
    (keyword (namespace attr) (str "_" (name attr)))))

(declare resolve-tuple-refs)

(defn ^:option<int> entid
  [^datascript.db/DB db
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  {:pre [(db? db)]}
  (match entity-ref
    (Datascript_runtime.Data_value.Entity_id eid)
    (if (pos? eid)
      (if (> eid emax)
        (raise (Invalid_argument
                "Entity id exceeds the supported range"))
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
          "Lookup ref attribute must be :db/unique")))
      (let [value (if (tuple? db attr)
                    (resolve-tuple-refs db attr value)
                    value)]
      (if-some [datom (fsearch db nil attr (Some value) nil)]
        (Some (.-e datom))
        None)))
    Datascript_runtime.Data_value.Current_tx
    None
    (Datascript_runtime.Data_value.Temp_id _)
    None))

(defn ^boolean numeric-eid-exists?
  [^datascript.db/DB db ^:int eid]
  (= eid
     (-> (-seek-datoms
          db :eavt
          (Some (Datascript_runtime.Data_value.Int eid))
          nil nil nil)
         first
         :e)))

(defn ^:int entid-strict
  [^datascript.db/DB db
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (if-some [eid (entid db entity-ref)]
    eid
    (raise (Invalid_argument
            "Nothing found for entity reference"))))

(defn ^:option<int> entid-some
  [^datascript.db/DB db
   ^:option<Datascript_runtime.Data_value.entity_ref> entity-ref]
  (if-some [entity-ref entity-ref]
    (Some (entid-strict db entity-ref))
    None))

(defn ^:vector<string> schema-tuple-attrs
  [^datascript.db/DB db ^:keyword tuple-attr]
  (if-some [entry (get (:schema db) tuple-attr)]
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

(defn ^:vector<option<Datascript_runtime.Data_value.t>> data-value-tuple-items
  [^:Datascript_runtime.Data_value.t value]
  (if-some [items (Datascript_runtime.Data_value.tuple_items value)]
    items
    (raise (Invalid_argument
            "Expected a DataScript tuple value"))))

(def ^:vector<int> empty-entity-ids
  [])

(defn ^:Datascript_runtime.Data_value.t resolve-tuple-refs
  [^datascript.db/DB db
   ^:keyword tuple-attr
   ^:Datascript_runtime.Data_value.t value]
  (let [^:vector<string> attrs (schema-tuple-attrs db tuple-attr)
        ^:vector<string> ref-attrs
        (Rrbvec.of_list
         (Lg_runtime.Lg_set.String_set.elements
          (-attrs-by db :db.type/ref)))
        ^:vector<Datascript_runtime.Data_value.entity_ref> entity-refs
        (Datascript_runtime.Data_value.tuple_entity_refs
         attrs ref-attrs value)
        ^:vector<int> eids
        (loop [^:int idx 0
               ^:vector<int> eids empty-entity-ids]
          (if (< idx (count entity-refs))
            (recur
             (inc idx)
             (conj eids
                   (entid-strict db (Rrbvec.nth entity-refs idx))))
            eids))]
    (Datascript_runtime.Data_value.resolve_tuple_refs
     attrs ref-attrs eids value)))

;;;;;;;;;; Transacting

(defn validate-datom [^datascript.db/DB db ^Datom datom]
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

(defn- current-tx ^long [^datascript.db/TxReport report]
  (inc (.-max-tx (.-db-before report))))

(defn- next-eid ^long [^datascript.db/DB db]
  (inc (.-max-eid db)))

(defn- ^boolean tempid? [^:int eid]
  (neg? eid))

(defn- ^boolean new-eid? [^datascript.db/DB db ^:int eid]
  (and (> eid (.-max-eid db))
       (< eid tx0))) ;; tx0 is max eid

(defn- ^datascript.db/DB advance-max-eid
  [^datascript.db/DB db ^:int eid]
  (cond-> db
    (new-eid? db eid)
    (assoc :max-eid eid)))

(defn ^:keyword schema-ident-value
  [^:Datascript_runtime.Data_value.t value]
  (match value
    (Datascript_runtime.Data_value.Keyword ident)
    (keyword ident)
    _ (raise (Invalid_argument
              "Schema :db/ident must be a keyword"))))

(defn remove-schema [^datascript.db/DB db ^Datom datom]
  (let [schema        (:schema db)
        schema-idents (:schema-idents db)
        schema-drafts (:schema-drafts db)
        eid           (.-e datom)
        attr          (.-a datom)]
    (if (= attr :db/ident)
      (let [ident (schema-ident-value (.-v datom))]
        (if-some [entry (get schema ident)]
          (assoc db
                 :schema (dissoc schema ident)
                 :schema-idents (dissoc schema-idents eid)
                 :schema-drafts
                 (assoc schema-drafts eid (dissoc entry :db/ident)))
          (util/raise "Schema with attribute " ident " does not exist"
                      {:error :retract/schema
                       :attribute ident})))
      (if-some [ident (get schema-idents eid)]
        (if-some [entry (get schema ident)]
          (assoc db :schema
                 (assoc schema ident (dissoc entry attr)))
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

(defn ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> get-schema
  [^datascript.db/DB db]
  (:schema db))

(defn update-schema [^datascript.db/DB db ^Datom datom]
  (let [schema        (:schema db)
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
               :schema (assoc schema ident entry)
               :schema-idents (assoc schema-idents eid ident)
               :schema-drafts (dissoc schema-drafts eid)))
      (if-some [ident (get schema-idents eid)]
        (let [entry (or (get schema ident) empty-schema-entry)]
          (assoc db :schema
                 (assoc schema ident (assoc entry attr value))))
        (let [draft (or (get schema-drafts eid) empty-schema-entry)]
          (assoc db :schema-drafts
                 (assoc schema-drafts eid (assoc draft attr value))))))))

(defn ^datascript.db/DB update-rschema [^datascript.db/DB db]
  (assoc db :rschema
         (rschema (merge-schema implicit-schema (get-schema db)))))

;; In context of `with-datom` we can use faster comparators which
;; do not check for nil (~10-15% performance gain in `transact`)

(defn ^datascript.db/DB with-datom [^datascript.db/DB db ^Datom datom]
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
        true      (assoc :hash (atom 0))
        schema?   (-> (update-schema datom)
                      update-rschema))
      (if-some [removing (fsearch db (.-e datom) attr (.-v datom) nil)]
        (cond-> db
          true      (update :eavt set/disj removing cmp-datoms-eavt-quick)
          true      (update :aevt set/disj removing cmp-datoms-aevt-quick)
          indexing? (update :avet set/disj removing cmp-datoms-avet-quick)
          true      (assoc :hash (atom 0))
          schema?   (-> (remove-schema datom) update-rschema))
        db))))

(defn ^:int schema-tuple-arity
  [^datascript.db/DB db ^:keyword tuple-attr]
  (if-some [entry (get (:schema db) tuple-attr)]
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

(defn ^:map<keyword;vector<option<Datascript_runtime.Data_value.t>>> empty-queued-tuples []
  {})

(defn- ^:map<keyword;vector<option<Datascript_runtime.Data_value.t>>> queue-tuple
  [^:map<keyword;vector<option<Datascript_runtime.Data_value.t>>> queue
   ^:keyword tuple-attr
   ^:int idx
   ^datascript.db/DB db
   ^:int eid
   ^:option<Datascript_runtime.Data_value.t> value]
  (let [tuple-value
        (if-some [queued (get queue tuple-attr)]
          queued
          (if-some [existing (fsearch db eid tuple-attr nil nil)]
            (data-value-tuple-items (.-v existing))
            (vec (repeat (schema-tuple-arity db tuple-attr) None))))
        tuple-value' (assoc tuple-value idx value)]
    (assoc queue tuple-attr tuple-value')))

(defn- ^:map<keyword;vector<option<Datascript_runtime.Data_value.t>>> queue-tuples
  [^:map<keyword;vector<option<Datascript_runtime.Data_value.t>>> queue
   ^:map<keyword;int> tuples
   ^datascript.db/DB db
   ^:int eid
   ^:option<Datascript_runtime.Data_value.t> value]
  (reduce-kv
   (fn [^:map<keyword;vector<option<Datascript_runtime.Data_value.t>>> queue
        ^:keyword tuple-attr
        ^:int idx]
     (queue-tuple queue tuple-attr idx db eid value))
   queue
   tuples))

(defn- ^datascript.db/TxReport transact-report [^datascript.db/TxReport report ^Datom datom]
  (let [db      (:db-after report)
        a       (:a datom)
        report'
        (assoc
         (assoc report :db-after (with-datom db datom))
         :tx-data
         (conj (.-tx-data report) datom))]
    (if (tuple-source? db a)
      (let [e      (:e datom)
            v      (if (datom-added datom) (:v datom) nil)
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

(defn- ^:tuple<map<keyword;Datascript_runtime.Data_value.t>;map<keyword;map<Datascript_runtime.Data_value.t;int>>> resolve-upserts
  "Returns a tuple of the remaining entity attributes and resolved upserts.
   Upsert attributes that resolve to existing entities
   are removed from entity, rest are kept in entity for insertion. No validation is performed.

   upserts :: {:name  {\"Ivan\"  1}
               :email {\"ivan@\" 2}
               :alias {\"abc\"   3
                       \"def\"   4}}}"
  [^datascript.db/DB db
   ^:map<keyword;Datascript_runtime.Data_value.t> entity]
  (if-some [idents (not-empty (-attrs-by db :db.unique/identity))]
    (let [resolve
          (fn [^:keyword a ^:Datascript_runtime.Data_value.t v]
            (if (ref? db a)
              (if-some
                [entity-ref
                 (Datascript_runtime.Data_value.entity_ref_value v)]
                (if-some [eid (entid db entity-ref)]
                  (if-some
                    [datom
                     (fsearch
                      db nil a
                      (Some (Datascript_runtime.Data_value.Ref eid))
                      nil)]
                    (Some (.-e datom))
                    None)
                  None)
                None)
              (if-some
                [datom
                 (fsearch db nil a (Some v) nil)]
                (Some (.-e datom))
                None)))
          split
          (fn [^:keyword a
               ^:vector<Datascript_runtime.Data_value.t> vs]
                    (reduce
                     (fn
                       [^:tuple<vector<Datascript_runtime.Data_value.t>;map<Datascript_runtime.Data_value.t;int>> acc
                        ^:Datascript_runtime.Data_value.t v]
                       (let [insert (tuple-get acc 0)
                             upsert (tuple-get acc 1)]
                         (if-some [e (resolve a v)]
                           (tuple insert (assoc upsert v e))
                           (tuple (conj insert v) upsert))))
                     (tuple [] (empty-value-eids)) vs))]
      (reduce-kv
       (fn
         [^:tuple<map<keyword;Datascript_runtime.Data_value.t>;map<keyword;map<Datascript_runtime.Data_value.t;int>>> acc
          ^:keyword a
          ^:Datascript_runtime.Data_value.t v]
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

(defn ^:option<int> validate-upsert-resolution
  [^:map<keyword;Datascript_runtime.Data_value.t> entity
   ^:option<tuple<int;keyword;Datascript_runtime.Data_value.t>> resolution]
  (if-some [resolved resolution]
    (let [upsert-id (tuple-get resolved 0)
          attr      (tuple-get resolved 1)
          value     (tuple-get resolved 2)
          eid-value (:db/id entity)]
      (if-some
        [entity-ref
         (if-some [eid-value eid-value]
           (Datascript_runtime.Data_value.entity_ref_value eid-value)
           None)]
        (match entity-ref
          (Datascript_runtime.Data_value.Entity_id eid)
          (when (and
                 (not (tempid? eid))
                 (not= upsert-id eid))
            (util/raise "Conflicting upsert: attribute " attr " value " value
                        " resolves to " (Int.to_string upsert-id)
                        ", but entity already has :db/id " (Int.to_string eid)
                        {:error     :transact/upsert
                         :assertion (tuple upsert-id attr value)
                         :conflict  {:db/id eid-value}}))
          _ nil)
        nil)
      (Some upsert-id))
    None))

(defn validate-upserts
  "Throws if not all upserts point to the same entity.
   Returns single eid that all upserts point to, or null."
  [^:map<keyword;Datascript_runtime.Data_value.t> entity
   ^:map<keyword;map<Datascript_runtime.Data_value.t;int>> upserts]
  (validate-upsert-resolution entity (collect-upsert-resolution upserts)))

(defn check-schema-update
  [^:map<keyword;Datascript_runtime.Data_value.t> entity]
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

(defn ^:vector<Datascript_runtime.Data_value.t> singleton-data-value
  [^:Datascript_runtime.Data_value.t value]
  [value])

(defn ^boolean data-value-lookup-ref?
  [^datascript.db/DB db
   ^:vector<Datascript_runtime.Data_value.t> values]
  (and
   (= (count values) 2)
   (if-some [first-value (first values)]
     (if-some [attr (Datascript_runtime.Data_value.keyword_value first-value)]
       (is-attr? db (keyword attr) :db.unique/identity)
       false)
     false)))

(defn- ^:vector<Datascript_runtime.Data_value.t> maybe-wrap-multival
  [^datascript.db/DB db
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
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

(defn ^:Datascript_runtime.Data_value.entity_ref data-value-entity-ref
  [^:Datascript_runtime.Data_value.t value]
  (if-some [entity-ref
            (Datascript_runtime.Data_value.entity_ref_value value)]
    entity-ref
    (raise (Invalid_argument
            "Expected a DataScript entity reference"))))

(defn ^:option<map<keyword;Datascript_runtime.Data_value.t>>
  data-value-entity-map
  [^:Datascript_runtime.Data_value.t value]
  (Datascript_runtime.Data_value.keyword_map_value value))

(defn ^:vector<tx-entry> empty-tx-entries []
  [])

(defn ^:vector<tx-entry> explode-attribute
  [^datascript.db/DB db
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:vector<tx-entry> entries
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (if (= attr :db/id)
    entries
    (let [reverse?   (reverse-ref? attr)
          straight-a (if reverse? (reverse-ref attr) attr)]
      (validate-attr attr (tuple entity-ref attr value))
      (when (and reverse? (not (ref? db straight-a)))
        (raise
         (Invalid_argument
          "Reverse attribute requires :db/valueType :db.type/ref")))
      (reduce
       (fn [^:vector<tx-entry> entries
            ^:Datascript_runtime.Data_value.t item]
         (conj
          entries
          (if-some
            [nested
             (if (ref? db straight-a)
               (data-value-entity-map item)
               None)]
            (TxEntity
             (assoc
              nested
              (reverse-ref attr)
              (Datascript_runtime.Data_value.Ref_to entity-ref)))
            (if reverse?
              (TxAdd (data-value-entity-ref item)
                     straight-a
                     (Datascript_runtime.Data_value.Ref_to entity-ref)
                     None)
              (TxAdd entity-ref straight-a item None)))))
       entries
       (maybe-wrap-multival db attr value)))))

(defn ^:vector<tx-entry> explode-pass
  [^datascript.db/DB db
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:map<keyword;Datascript_runtime.Data_value.t> entity
   ^boolean tuple-pass?
   ^:vector<tx-entry> entries]
  (reduce-kv
   (fn [^:vector<tx-entry> entries
        ^:keyword attr
        ^:Datascript_runtime.Data_value.t value]
     (if (= tuple-pass? (tuple? db attr))
       (explode-attribute db entity-ref entries attr value)
       entries))
   entries
   entity))

(defn- ^:vector<tx-entry> explode
  [^datascript.db/DB db
   ^:map<keyword;Datascript_runtime.Data_value.t> entity]
  (if-some [id-value (get entity :db/id)]
    (let [entity-ref (data-value-entity-ref id-value)
          entries (explode-pass db entity-ref entity false
                                (empty-tx-entries))]
      (explode-pass db entity-ref entity true entries))
    (raise (Invalid_argument
            "Transaction entity requires :db/id"))))

(defn- ^datascript.db/TxReport transact-add
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value
   ^:option<int> transaction]
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
        old-datom ^Datom (if multival?
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

(defn- ^datascript.db/TxReport transact-retract-datom
  [^datascript.db/TxReport report ^Datom d]
  (let [tx (current-tx report)]
    (transact-report report (datom (.-e d) (.-a d) (.-v d) tx false))))

(defn- ^:option<tx-entry> component-retraction
  [^datascript.db/DB db ^Datom datom]
  (if (component? db (.-a datom))
    (if-some [eid (Datascript_runtime.Data_value.ref_value
                   (.-v datom))]
      (Some (tx-retract-entity-id eid))
      (raise
       (Invalid_argument
        "Component attribute value must be a resolved reference")))
    None))

(defn- ^:vector<tx-entry> retract-components
  [^datascript.db/DB db ^:vector<Datom> datoms]
  (Rrbvec.of_list
   (List.filter_map
    (fn [^Datom datom]
      (component-retraction db datom))
    (Rrbvec.to_list datoms))))

(defn ^:option<Datascript_runtime.Data_value.t> queued-tuple-value
  [^:vector<option<Datascript_runtime.Data_value.t>> values]
  (if (every?
       (fn [^:option<Datascript_runtime.Data_value.t> value]
         (match value
           None true
           (Some _) false))
       values)
    None
    (Some (Datascript_runtime.Data_value.tuple_of_vector values))))

(defn ^:vector<tx-entry> flush-tuples [^datascript.db/TxReport report]
  (let [db (:db-after report)]
    (reduce-kv
     (fn [^:vector<tx-entry> entities
          ^:int eid
          ^:map<keyword;vector<option<Datascript_runtime.Data_value.t>>> tuples+values]
       (reduce-kv
        (fn [^:vector<tx-entry> entities
             ^:keyword tuple-attr
             ^:vector<option<Datascript_runtime.Data_value.t>> values]
          (let [value   (queued-tuple-value values)
                current (if-some [datom (fsearch db eid tuple-attr nil nil)]
                          (Some (.-v datom))
                          None)
                entity-ref (Datascript_runtime.Data_value.Entity_id eid)]
            (cond
              (= value current) entities
              (nil? value)
              (conj entities
                    (TxSetTuple entity-ref tuple-attr None))
              :else
              (if-some [tuple-value value]
                (conj entities
                      (TxSetTuple
                       entity-ref tuple-attr (Some tuple-value)))
                entities))))
        entities
        tuples+values))
     (empty-tx-entries)
     (:queued-tuples report))))

(defn ^:Datascript_runtime.Data_value.t entity-ref-key
  [^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (Datascript_runtime.Data_value.Ref_to entity-ref))

(defn ^:tuple<datascript.db/TxReport;Datascript_runtime.Data_value.entity_ref> allocate-tx-entity
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
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
                    (.-value-tempids report))]
        (tuple report (Datascript_runtime.Data_value.Entity_id eid))))))

(defn ^:tuple<datascript.db/TxReport;Datascript_runtime.Data_value.entity_ref> resolve-tx-entity
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (match entity-ref
    Datascript_runtime.Data_value.Current_tx
    (tuple report
           (Datascript_runtime.Data_value.Entity_id
            (current-tx report)))
    (Datascript_runtime.Data_value.Entity_id eid)
    (if (pos? eid)
      (tuple (update report :db-after advance-max-eid eid)
             entity-ref)
      (allocate-tx-entity report entity-ref))
    (Datascript_runtime.Data_value.Temp_id _)
    (allocate-tx-entity report entity-ref)
    (Datascript_runtime.Data_value.Ident _)
    (tuple report
           (Datascript_runtime.Data_value.Entity_id
            (entid-strict (:db-after report) entity-ref)))
    (Datascript_runtime.Data_value.Lookup_ref _ _)
    (tuple report
           (Datascript_runtime.Data_value.Entity_id
            (entid-strict (:db-after report) entity-ref)))))

(defn ^:option<int> existing-tx-eid
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (match entity-ref
    (Datascript_runtime.Data_value.Entity_id eid)
    (if (pos? eid) (Some eid)
        (get (:tempids report) (entity-ref-key entity-ref)))
    (Datascript_runtime.Data_value.Temp_id _)
    (get (:tempids report) (entity-ref-key entity-ref))
    Datascript_runtime.Data_value.Current_tx
    (Some (current-tx report))
    (Datascript_runtime.Data_value.Ident _)
    (entid (:db-after report) entity-ref)
    (Datascript_runtime.Data_value.Lookup_ref _ _)
    (entid (:db-after report) entity-ref)))

(defn ^boolean temp-entity-ref?
  [^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (match entity-ref
    (Datascript_runtime.Data_value.Entity_id eid) (not (pos? eid))
    (Datascript_runtime.Data_value.Temp_id _) true
    _ false))

(defn reject-tempid-operation
  [^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (when (temp-entity-ref? entity-ref)
    (raise
     (Invalid_argument
      "Tempids are allowed in transaction add operations only"))))

(defn ^datascript.db/TxReport mark-entity-used
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref resolved-ref]
  (let [eid (entid-strict (:db-after report) resolved-ref)]
    (assoc report :value-tempids
           (dissoc (:value-tempids report) eid))))

(defn ^:tuple<datascript.db/TxReport;Datascript_runtime.Data_value.t> resolve-tx-value
  [^datascript.db/TxReport report
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (let [db (:db-after report)]
    (if (ref? db attr)
      (let [entity-ref (data-value-entity-ref value)
            key (entity-ref-key entity-ref)
            existing (if (temp-entity-ref? entity-ref)
                       (get (:tempids report) key)
                       None)
            result (resolve-tx-entity report entity-ref)
            report (tuple-get result 0)
            resolved-ref (tuple-get result 1)
            eid (entid-strict (:db-after report) resolved-ref)]
        (tuple
         (if (temp-entity-ref? entity-ref)
           (if-some [_existing existing]
             report
             (assoc report :value-tempids
                    (assoc
                     (:value-tempids report)
                     eid
                     key)))
           report)
         (Datascript_runtime.Data_value.Ref eid)))
      (tuple report
             (if (tuple? db attr)
               (resolve-tuple-refs db attr value)
               value)))))

(defn ^:tuple<datascript.db/TxReport;bool> direct-tuple-match
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:keyword tuple-attr
   ^:Datascript_runtime.Data_value.t value]
  (let [entity-result (resolve-tx-entity report entity-ref)
        report (tuple-get entity-result 0)
        resolved-ref (tuple-get entity-result 1)
        report (mark-entity-used report resolved-ref)
        db (:db-after report)
        eid (entid-strict db resolved-ref)
        value (resolve-tuple-refs db tuple-attr value)
        attrs (schema-tuple-attrs db tuple-attr)
        items (data-value-tuple-items value)
        matches?
        (if (= (count attrs) (count items))
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
              true))
          false)]
    (tuple report matches?)))

(defn ^datascript.db/TxReport transact-add-entry
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value
   ^:option<int> transaction]
  (let [temp-entity? (temp-entity-ref? entity-ref)
        value-result (resolve-tx-value report attr value)
        report (tuple-get value-result 0)
        value (tuple-get value-result 1)
        entity-result (resolve-tx-entity report entity-ref)
        report (tuple-get entity-result 0)
        entity-ref (tuple-get entity-result 1)
        report (mark-entity-used report entity-ref)
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

(defn ^datascript.db/TxReport transact-retract-entry
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:keyword attr
   ^:option<Datascript_runtime.Data_value.t> value]
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
               (Some eid) (Some attr) nil nil)))
    report))

(defn ^:vector<Datom> incoming-reference-datoms
  [^datascript.db/DB db ^:int eid]
  (reduce
   (fn [^:vector<Datom> datoms ^:keyword attr]
     (into
      datoms
      (search-vector
       db nil (Some attr)
       (Some (Datascript_runtime.Data_value.Ref eid))
       nil)))
   []
   (Lg_runtime.Lg_set.String_set.elements
    (-attrs-by db :db.type/ref))))

(defn ^datascript.db/TxReport transact-cas-entry
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:keyword attr
   ^:option<Datascript_runtime.Data_value.t> old-value
   ^:Datascript_runtime.Data_value.t new-value]
  (if-some [eid (existing-tx-eid report entity-ref)]
    (let [db (:db-after report)
          resolve-value
          (fn [^:Datascript_runtime.Data_value.t value]
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
          matches?
          (if (multival? db attr)
            (if-some [expected old-value]
              (loop [datoms
                     (search-vector db (Some eid) (Some attr) nil nil)
                     index 0]
                (if (< index (count datoms))
                  (if (= (.-v (nth datoms index)) expected)
                    true
                    (recur datoms (inc index)))
                  false))
              false)
            (let [current (fsearch db eid attr nil nil)]
              (if-some [expected old-value]
                (if-some [datom current]
                  (= (.-v datom) expected)
                  false)
                (nil? current))))]
      (if matches?
        (transact-add
         report
         (Datascript_runtime.Data_value.Entity_id eid)
         attr new-value None)
        (raise (Invalid_argument
                "Compare-and-set transaction failed"))))
    (raise (Invalid_argument
            "Compare-and-set entity does not exist"))))

(defn ^datascript.db/TxReport finish-transaction
  [^datascript.db/TxReport report]
  (if (empty? (:value-tempids report))
    (let [current (current-tx report)]
      (assoc
       (assoc
        report
        :tempids
        (assoc
         (.-tempids report)
         (entity-ref-key
          (Datascript_runtime.Data_value.Current_tx))
         current))
       :db-after
       (assoc
        (.-db-after report)
        :max-tx
        (inc (.-max-tx (.-db-after report))))))
    (raise
     (Invalid_argument
      "Tempids used only as values in transaction"))))

(defn- ^:vector<tuple<vector<tx-entry>;int>> push-tx-continuation
  [^:vector<tuple<vector<tx-entry>;int>> continuations
   ^:vector<tx-entry> entries
   ^:int entry-index]
  (into [(tuple entries entry-index)] continuations))

(defn- ^:tuple<datascript.db/TxReport;vector<tx-entry>;int;vector<tuple<vector<tx-entry>;int>>>
  continue-with-generated
  [^datascript.db/TxReport report
   ^:vector<tx-entry> generated
   ^:vector<tx-entry> entries
   ^:int next-index
   ^:vector<tuple<vector<tx-entry>;int>> continuations]
  (if (= 0 (count generated))
    (tuple report entries next-index continuations)
    (tuple
     report
     generated
     0
     (push-tx-continuation continuations entries next-index))))

(defn- ^:tuple<datascript.db/TxReport;vector<tx-entry>;int;vector<tuple<vector<tx-entry>;int>>>
  retract-attribute-next
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:keyword attr
   ^:vector<tx-entry> entries
   ^:int next-index
   ^:vector<tuple<vector<tx-entry>;int>> continuations]
  (if-some [eid (existing-tx-eid report entity-ref)]
    (let [db (:db-after report)
          datoms (search-vector db (Some eid) (Some attr) nil nil)
          generated (retract-components db datoms)
          report (reduce transact-retract-datom report datoms)]
      (continue-with-generated
       report generated entries next-index continuations))
    (tuple report entries next-index continuations)))

(defn- ^:tuple<datascript.db/TxReport;vector<tx-entry>;int;vector<tuple<vector<tx-entry>;int>>>
  retract-entity-next
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:vector<tx-entry> entries
   ^:int next-index
   ^:vector<tuple<vector<tx-entry>;int>> continuations]
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

(defn- ^datascript.db/TxReport bind-upserted-entity
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:int upserted-eid]
  (match entity-ref
    (Datascript_runtime.Data_value.Entity_id eid)
    (if (pos? eid)
      (if (= eid upserted-eid)
        report
        (raise (Invalid_argument "Conflicting upsert entity id")))
      (let [key (entity-ref-key entity-ref)]
        (if-some [existing (get (:tempids report) key)]
          (if (= existing upserted-eid)
            report
            (raise (Invalid_argument
                    "Conflicting upsert tempid resolution")))
          (assoc report :tempids
                 (assoc (:tempids report) key upserted-eid)))))
    (Datascript_runtime.Data_value.Temp_id _)
    (let [key (entity-ref-key entity-ref)]
      (if-some [existing (get (:tempids report) key)]
        (if (= existing upserted-eid)
          report
          (raise (Invalid_argument
                  "Conflicting upsert tempid resolution")))
        (assoc report :tempids
               (assoc (:tempids report) key upserted-eid))))
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

(defn- ^boolean upsert-retry-needed?
  [^datascript.db/TxReport report
   ^:Datascript_runtime.Data_value.entity_ref entity-ref
   ^:int upserted-eid]
  (match entity-ref
    (Datascript_runtime.Data_value.Entity_id eid)
    (if (pos? eid)
      false
      (if-some [existing
                (get (:tempids report) (entity-ref-key entity-ref))]
        (not= existing upserted-eid)
        false))
    (Datascript_runtime.Data_value.Temp_id _)
    (if-some [existing
              (get (:tempids report) (entity-ref-key entity-ref))]
      (not= existing upserted-eid)
      false)
    _ false))

(defn- ^:option<int> unique-upsert-eid
  [^datascript.db/TxReport report
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (let [db (:db-after report)]
    (if (is-attr? db attr :db.unique/identity)
      (let [resolved
            (if (ref? db attr)
              (if-some
                [entity-ref
                 (Datascript_runtime.Data_value.entity_ref_value value)]
                (if-some [eid (existing-tx-eid report entity-ref)]
                  (Some (Datascript_runtime.Data_value.Ref eid))
                  None)
                None)
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

(defn- ^:map<keyword;Datascript_runtime.Data_value.t> assoc-entity-id
  [^:map<keyword;Datascript_runtime.Data_value.t> entity ^:int eid]
  (assoc
   entity
   :db/id
   (Datascript_runtime.Data_value.Ref_to
    (Datascript_runtime.Data_value.Entity_id eid))))

(defn- ^:tuple<datascript.db/TxReport;vector<tx-entry>;int;vector<tuple<vector<tx-entry>;int>>>
  expand-entity-next
  [^datascript.db/TxReport report
   ^:map<keyword;Datascript_runtime.Data_value.t> entity
   ^:vector<tx-entry> entries
   ^:int next-index
   ^:vector<tuple<vector<tx-entry>;int>> continuations]
  (if-some [id-value (get entity :db/id)]
    (if-some
      [entity-ref
       (Datascript_runtime.Data_value.entity_ref_value id-value)]
      (let [resolved (resolve-tx-entity report entity-ref)
            report (tuple-get resolved 0)
            entity-ref (tuple-get resolved 1)
            report (mark-entity-used report entity-ref)
            entity
            (assoc
             entity
             :db/id
             (Datascript_runtime.Data_value.Ref_to entity-ref))]
        (tuple
         report
         (explode (:db-after report) entity)
         0
         (push-tx-continuation
          continuations entries next-index)))
      (raise
       (Invalid_argument
        "Transaction entity has an invalid :db/id")))
    (raise
     (Invalid_argument "Transaction entity requires :db/id"))))

(type-variant tx-step-result
  (TxStep
   :tuple<datascript.db/TxReport;vector<tx-entry>;int;vector<tuple<vector<tx-entry>;int>>>)
  (TxRestart :Datascript_runtime.Data_value.entity_ref :int))

(type-variant tx-run-result
  (TxFinished :datascript.db/TxReport)
  (TxRetry :Datascript_runtime.Data_value.entity_ref :int))

(defn ^tx-run-result transact-tx-data-loop
  [^datascript.db/TxReport report
   ^:vector<tx-entry> entries
   ^:int entry-index
   ^:vector<tuple<vector<tx-entry>;int>> continuations]
  (if (< entry-index (count entries))
      (let [entry (nth entries entry-index)
            next-index (inc entry-index)
            ^tx-step-result step
            (match entry
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

              (TxEntity entity)
              (let [_ (check-schema-update entity)
                    db (:db-after report)
                    upsert-result (resolve-upserts db entity)
                    entity (tuple-get upsert-result 0)
                    upserts (tuple-get upsert-result 1)
                    upserted-eid (validate-upserts entity upserts)]
                (if-some [eid upserted-eid]
                  (if-some [id-value (get entity :db/id)]
                    (if-some
                      [entity-ref
                       (Datascript_runtime.Data_value.entity_ref_value
                        id-value)]
                      (if (upsert-retry-needed?
                           report entity-ref eid)
                        (TxRestart entity-ref eid)
                        (let [report
                              (bind-upserted-entity
                               report entity-ref eid)
                              entity (assoc-entity-id entity eid)]
                          (TxStep
                           (expand-entity-next
                            report entity entries next-index
                            continuations))))
                      (raise
                       (Invalid_argument
                        "Transaction entity has an invalid :db/id")))
                    (TxStep
                     (expand-entity-next
                      report
                      (assoc-entity-id entity eid)
                      entries next-index continuations)))
                  (let [entity
                        (if (contains? entity :db/id)
                          entity
                          (assoc-entity-id entity (next-eid db)))]
                    (TxStep
                     (expand-entity-next
                      report entity entries next-index continuations)))))

              (TxAdd entity-ref attr value transaction)
              (if (tuple? (:db-after report) attr)
                (let [result
                      (direct-tuple-match
                       report entity-ref attr value)
                      report (tuple-get result 0)]
                  (if (tuple-get result 1)
                    (TxStep
                     (tuple report entries next-index continuations))
                    (raise
                     (Invalid_argument
                      "Cannot modify tuple attributes directly"))))
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
                          "Cannot modify tuple attributes directly"))))
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
              (TxStep
               (tuple
                (if-some [tuple-value value]
                  (transact-add-entry
                   report entity-ref attr tuple-value None)
                  (transact-retract-entry
                   report entity-ref attr None))
                entries
                next-index
                continuations))

              (TxCas entity-ref attr old-value new-value)
              (let [_ (reject-tempid-operation entity-ref)]
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
          (TxRetry entity-ref eid)))
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

(defn ^tx-run-result transact-tx-data-run
  [^datascript.db/TxReport initial-report
   ^:vector<tx-entry> initial-entries]
  (transact-tx-data-loop initial-report initial-entries 0 []))

(defn ^datascript.db/TxReport transact-tx-data-impl
  [^datascript.db/TxReport initial-report
   ^:vector<tx-entry> initial-entries]
  (match
    (transact-tx-data-run initial-report initial-entries)
    (TxFinished report)
    report
    (TxRetry entity-ref upserted-eid)
    (let [key (entity-ref-key entity-ref)]
      (if-some [existing (get (:tempids initial-report) key)]
        (if (= existing upserted-eid)
          (raise
           (Invalid_argument
            "Upsert retry repeated without making progress"))
          (raise
           (Invalid_argument
            "Conflicting upsert tempid resolution")))
        (transact-tx-data-impl
         (assoc
          initial-report
          :tempids
          (assoc (:tempids initial-report)
                 key upserted-eid))
         initial-entries)))))

(defn ^datascript.db/TxReport transact-tx-data
  [^datascript.db/TxReport report
   ^:vector<tx-entry> entries]
  (transact-tx-data-impl report entries))
