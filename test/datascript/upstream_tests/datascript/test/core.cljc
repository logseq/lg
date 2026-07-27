(ns datascript.test.core
  (:require
   [clojure.test :refer [deftest is]]
   [datascript.core :as d]))

(defn datom-equal?
  [^datascript.db/Datom left ^datascript.db/Datom right]
  (and
   (= (.-e left) (.-e right))
   (= (.-a left) (.-a right))
   (Datascript_runtime.Data_value.equal (.-v left) (.-v right))))

(defn datom-vector-contains?
  [^:vector<datascript.db/Datom> datoms
   ^datascript.db/Datom expected]
  (loop [index 0]
    (if (< index (count datoms))
      (if (datom-equal? (nth datoms index) expected)
        true
        (recur (inc index)))
      false)))

(defn datom-sets-equal?
  [^:vector<datascript.db/Datom> actual
   ^:vector<datascript.db/Datom> expected]
  (and
   (= (count actual) (count expected))
   (every?
    (fn [^datascript.db/Datom datom]
      (datom-vector-contains? actual datom))
    expected)))

(deftest test-protocols
  (let [database
        (d/db-with
         (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
         [{:db/id 1 :name "Ivan" :aka ["IV" "Terrible"]}
          {:db/id 2 :name "Petr" :age 37 :huh? false}])]
    (is (= (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
           (empty database)))
    (is (= 6 (count database)))
    (is
     (=
      #{:schema
        :schema-idents
        :schema-drafts
        :eavt
        :aevt
        :avet
        :max-eid
        :max-tx
        :rschema
        :tx-functions
        :identity-hash
        :hash
        :metadata}
      (set (keys database))))
    (is (map? database))
    (is (seqable? (:eavt database)))
    (is
     (datom-sets-equal?
      (vec (seq (:eavt database)))
      [(d/datom 1 :aka "IV")
       (d/datom 1 :aka "Terrible")
       (d/datom 1 :name "Ivan")
       (d/datom 2 :age 37)
       (d/datom 2 :name "Petr")
       (d/datom 2 :huh? false)]))))

(deftest test-data-readers
  (let [expected (d/datom 1 :name "Ada" 17 false)
        reader-value
        (Datascript_runtime.Serialization_value.read_datom
         (pr-str expected))]
    (is
     (match (get d/data-readers (symbol "datascript/Datom"))
       None false
       (Some reader)
       (match reader
         (datascript.core/DatomDataReader read)
         (= expected (read reader-value))
         _ false))))

  (let [expected
        (->
         (d/empty-db {:name {:db/unique :db.unique/identity}})
         (d/db-with [[:db/add 1 :name "Ada"]]))
        reader-value
        (Datascript_runtime.Serialization_value.read_database
         (pr-str expected))]
    (is
     (match (get d/data-readers (symbol "datascript/DB"))
       None false
       (Some reader)
       (match reader
         (datascript.core/DatabaseDataReader read)
         (datascript.db/db-equal? expected (read reader-value))
         _ false)))))
