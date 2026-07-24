(ns datascript.test.filter-static
  (:require
   [clojure.test :refer [deftest is testing]]
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.test.core :as tdc]
   [me.tonsky.persistent-sorted-set :as set]))

(defn ^datascript.db/tx-entry person
  [^:int entity-id ^:string name ^:int age]
  (let [entity (db/empty-entity-map)]
    (db/tx-entity
     (assoc
      entity
      :db/id
      (tdc/entity-id-value entity-id)
      :name
      (tdc/string-value name)
      :age
      (tdc/int-value age)))))

(defn names
  [database]
  (mapv
   (fn [^datascript.db/Datom datom] (.-v datom))
    (d/datoms
     database
     :aevt
     (tdc/keyword-value :name))))

(deftest repeated-filter-preserves-upstream-composition
  (let [database
        (d/db-with
         (d/empty-db)
         [(person 1 "Petr" 32)
          (person 2 "Oleg" 22)
          (person 3 "Ivan" 12)])
        remove-oleg
        (fn [^datascript.db/DB _database ^datascript.db/Datom datom]
          (not= 2 (.-e datom)))
        remove-ivan
        (fn [^datascript.db/DB _database ^datascript.db/Datom datom]
          (not= 3 (.-e datom)))
        once (d/filter database remove-oleg)
        twice (d/filter once remove-ivan)]
    (testing "filtered values still implement the database API"
      (is (= [(tdc/string-value "Petr")
              (tdc/string-value "Ivan")]
             (names once)))
      (is (= [(tdc/string-value "Petr")]
             (names twice))))
    (testing "the view identity remains observable like upstream"
      (is (not (d/is-filtered database)))
      (is (d/is-filtered once))
      (is (d/is-filtered twice)))))

(deftest database-options-preserve-upstream-reference-semantics
  (let [strong-options
        (db/options-with-ref-type
         (db/default-options)
         (Lg_runtime.Runtime_ref_type.Strong))
        weak-options
        (db/options-with-ref-type
         (db/default-options)
         (Lg_runtime.Runtime_ref_type.Weak))
        datoms [(d/datom
                 1
                 :name
                 (tdc/string-value "Petr"))]
        default-db (d/empty-db)
        strong-db (d/init-db datoms db/empty-schema strong-options)
        weak-db (d/init-db datoms db/empty-schema weak-options)]
    (is (= (Lg_runtime.Runtime_ref_type.Weak)
           (:ref-type (set/settings (.-eavt default-db)))))
    (is (= (Lg_runtime.Runtime_ref_type.Strong)
           (:ref-type (set/settings (.-eavt strong-db)))))
    (is (= (Lg_runtime.Runtime_ref_type.Weak)
           (:ref-type (set/settings (.-eavt weak-db)))))))
