(ns datascript.test.api-smoke
  (:require [datascript.core :as d]
            [datascript.lg.query-types :as query-types]
            [datascript.util :as util]))

(def environment #?(:native "native" :melange "melange"))

(def database
  (d/init-db
   [(d/datom 1 :name "Ivan")
    (d/datom 1 :age 19)
    (d/datom 2 :name "Oleg")]
   {:age {:db/index true}}))

(def person (d/entity database 1))
(def pulled (d/pull database [:name :age] 1))
(def queried
  (d/q '[:find ?e ?name :where [?e :name ?name]] database))
(def filtered
  (d/filter database (fn [_ datom] (= :name (:a datom)))))
(def restored
  (d/from-serializable (d/serializable database)))

(def operation-db
  (d/db-with database [[:db/add 3 :name "Petr"]]))

(def operation-report
  (d/with
   operation-db
   [[:db/add 3 :age 30]]
   (zipmap
    [:source]
    [(Datascript_runtime.Data_value.String "api-smoke")])))

(def person-matches?
  (if-some [entity person]
    (match (:name entity)
      (Some
       (datascript.impl.entity/EntityScalar
        (Datascript_runtime.Data_value.String value)))
      (= "Ivan" value)
      _ false)
    false))

(def pull-matches?
  (if-some [result pulled]
    (if-some
      [value
       (get
        result
        (Datascript_runtime.Data_value.Keyword ":age"))]
      (Datascript_runtime.Data_value.equal
       value
       (Datascript_runtime.Data_value.Int 19))
      false)
    false))

(defn query-row-matches?
  [^:array<datascript.lg.query-types/result> row
   ^int entity-id
   ^:string name]
  (and
   (match (aget row 0)
     (Datascript_runtime.Query_value.Entity actual)
     (= entity-id actual)
     _ false)
   (match (aget row 1)
     (Datascript_runtime.Query_value.Value
      (Datascript_runtime.Data_value.String actual))
     (= name actual)
     _ false)))

(def query-matches?
  (if-some [rows (query-types/output-relation queried)]
    (and
     (= 2 (count rows))
     (let [first-row (nth rows 0)
           second-row (nth rows 1)]
       (or
        (and
         (query-row-matches? first-row 1 "Ivan")
         (query-row-matches? second-row 2 "Oleg"))
        (and
         (query-row-matches? first-row 2 "Oleg")
         (query-row-matches? second-row 1 "Ivan")))))
    false))

(println
 (str environment ":api-smoke:"
      (d/db? (d/empty-db)) ":"
      (d/datom? (d/datom 1 :name "Ivan")) ":"
      (= [1 2] (util/distinct-by (fn [value] (mod value 2)) [1 3 2])) ":"
      (if-some [found (util/find even? [1 2 3])]
        (= 2 found)
        false) ":"
      (= 7 (util/single [7])) ":"
      person-matches? ":"
      pull-matches? ":"
      query-matches? ":"
      (= 2 (count (d/datoms filtered :eavt))) ":"
      (if-some [datom (d/find-datom database :eavt 1 :name)]
        (Datascript_runtime.Data_value.equal
         (Datascript_runtime.Data_value.String "Ivan")
         (:v datom))
        false) ":"
      (= 3 (count (d/seek-datoms database :eavt 1))) ":"
      (= 1 (count (d/rseek-datoms database :avet :age 20))) ":"
      (= 1 (count (d/index-range database :age 18 20))) ":"
      (= 3 (count (d/datoms restored :eavt))) ":"
      (if-some [datom (d/find-datom operation-db :eavt 3 :name)]
        (Datascript_runtime.Data_value.equal
         (Datascript_runtime.Data_value.String "Petr")
         (:v datom))
        false) ":"
      (if-some [datom (d/find-datom (:db-after operation-report)
                                    :eavt 3 :age)]
        (Datascript_runtime.Data_value.equal
         (Datascript_runtime.Data_value.Int 30)
         (:v datom))
        false) ":"
      (if-some
        [source
         (Datascript_runtime.Data_value.keyword_map_get
          ":source"
          (.-tx-meta operation-report))]
        (Datascript_runtime.Data_value.equal
         (Datascript_runtime.Data_value.String "api-smoke")
         source)
        false)))
