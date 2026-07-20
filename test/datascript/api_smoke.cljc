(ns datascript.test.api-smoke
  (:require [datascript.core :as d]))

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

(println
 (str environment ":api-smoke:"
      (d/db? (d/empty-db)) ":"
      (= "Ivan" (:name person)) ":"
      (= 19 (:age pulled)) ":"
      (= #{[1 "Ivan"] [2 "Oleg"]} queried) ":"
      (= 2 (count (d/datoms filtered :eavt))) ":"
      (= "Ivan" (:v (d/find-datom database :eavt 1 :name))) ":"
      (= 3 (count (d/seek-datoms database :eavt 1))) ":"
      (= 1 (count (d/rseek-datoms database :avet :age 20))) ":"
      (= 1 (count (d/index-range database :age 18 20))) ":"
      (= 3 (count (d/datoms restored :eavt)))))
