(ns datascript.test.ident
  (:require
   [clojure.test :refer [deftest is]]
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.impl.entity :as entity]
   [datascript.lg.query-types :as query-types]
   [datascript.test.core :as tdc]))

(defn ^:Datascript_runtime.Data_value.t int-value [^:int value]
  (Datascript_runtime.Data_value.Int value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:string value]
  (Datascript_runtime.Data_value.Keyword value))

(def *db
  (delay
   (->
    (d/empty-db {:ref {:db/valueType :db.type/ref}})
    (d/db-with
     [[:db/add 1 :db/ident :ent1]
      [:db/add 2 :db/ident :ent2]
      [:db/add 2 :ref 1]]))))

(defn ^boolean query-scalar-int?
  [^datascript.lg.query-types/output output ^:int expected]
  (match (query-types/output-scalar output)
    (Some (Some result))
    (Datascript_runtime.Data_value.equal
     (tdc/query-result-value result)
     (int-value expected))
    _ false))

(deftest test-q
  (is
   (query-scalar-int?
    (d/q '[:find ?v .
           :where [:ent2 :ref ?v]]
         @*db)
    1))
  (is
   (query-scalar-int?
    (d/q '[:find ?f .
           :where [?f :ref :ent1]]
         @*db)
    2)))

(defn ^:option<datascript.impl.entity/Entity> entity-by-ident
  [^datascript.db/DB database ^:keyword ident]
  (d/entity-closed
   (db/database-view database)
   (Datascript_runtime.Data_value.Ident (str ident))))

(defn ^:map<keyword;datascript.impl.entity/EntityValue> empty-entity-map []
  {})

(deftest test-transact!
  (let [database
        (d/db-with
         @*db
         [[:db/add :ent1 :ref :ent2]])]
    (if-some [source (entity-by-ident database :ent1)]
      (match (entity/lookup-entity source :ref)
        (Some (entity/EntityReference target))
        (is (= 2 (.-eid target)))
        _ (is false))
      (is false))))

(deftest test-entity
  (if-some [value (entity-by-ident @*db :ent1)]
    (let [touched (entity/touch-entity value)]
      (is
       (=
        (->
         (empty-entity-map)
         (assoc
          :db/ident
          (entity/EntityScalar (keyword-value ":ent1"))))
        (into (empty-entity-map) touched))))
    (is false)))

(defn ^:map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>
  expected-pull []
  {(keyword-value ":db/id") (int-value 1)
   (keyword-value ":db/ident") (keyword-value ":ent1")})

(deftest test-pull
  (is
   (=
    (Some (expected-pull))
    (d/pull @*db '[*] :ent1))))
