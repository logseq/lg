(ns datascript.test.filter
  (:require
   [clojure.test :as t :refer [are deftest is testing]]
   [datascript.core :as d]
   [datascript.db :as db]))

(deftest test-filter-db
  (let [empty-db (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
        database
        (d/db-with
         empty-db
         [{:db/id 1
           :name "Petr"
           :email "petya@spb.ru"
           :aka ["I" "Great"]
           :password "<SECRET>"}
          {:db/id 2
           :name "Ivan"
           :aka ["Terrible" "IV"]
           :password "<PROTECTED>"}
          {:db/id 3
           :name "Nikolai"
           :aka ["II"]
           :password "<UNKWOWN>"}])
        remove-pass (fn [_ datom] (not= :password (:a datom)))
        remove-ivan (fn [_ datom] (not= 2 (:e datom)))
        long-akas
        (fn [unfiltered-db datom]
          (or (not= :aka (:a datom))
              (<= (count (:aka (d/entity unfiltered-db (:e datom)))) 1)
              (>= (count (:v datom)) 4)))]

    (are [filtered-db result]
         (= (d/q '[:find ?v :where [_ :password ?v]] filtered-db) result)
      database #{["<SECRET>"] ["<PROTECTED>"] ["<UNKWOWN>"]}
      (d/filter database remove-pass) #{}
      (d/filter database remove-ivan) #{["<SECRET>"] ["<UNKWOWN>"]}
      (-> database (d/filter remove-ivan) (d/filter remove-pass)) #{})

    (are [filtered-db result]
         (= (d/q '[:find ?v :where [_ :aka ?v]] filtered-db) result)
      database #{["I"] ["Great"] ["Terrible"] ["IV"] ["II"]}
      (d/filter database remove-pass) #{["I"] ["Great"] ["Terrible"] ["IV"] ["II"]}
      (d/filter database remove-ivan) #{["I"] ["Great"] ["II"]}
      (d/filter database long-akas) #{["Great"] ["Terrible"] ["II"]}
      (-> database (d/filter remove-ivan) (d/filter long-akas)) #{["Great"] ["II"]}
      (-> database (d/filter long-akas) (d/filter remove-ivan)) #{["Great"] ["II"]})

    (testing "Entities"
      (is (= (:password (d/entity database 1)) "<SECRET>"))
      (is (= (:password (d/entity (d/filter database remove-pass) 1)
                        ::not-found)
             ::not-found))
      (is (= (:aka (d/entity database 2)) #{"Terrible" "IV"}))
      (is (= (:aka (d/entity (d/filter database long-akas) 2))
             #{"Terrible"})))

    (testing "Index access"
      (is (= (map :v (d/datoms database :aevt :password))
             ["<SECRET>" "<PROTECTED>" "<UNKWOWN>"]))
      (is (= (map :v (d/datoms (d/filter database remove-pass)
                               :aevt :password))
             [])))

    (testing "equiv"
      (is (= (d/db-with database [[:db.fn/retractEntity 2]])
             (d/filter database remove-ivan)))
      (is (= empty-db
             (d/filter empty-db (constantly true))
             (d/filter database (constantly false)))))

    (testing "hash"
      (is (= (hash (d/db-with database [[:db.fn/retractEntity 2]]))
             (hash (d/filter database remove-ivan))))
      (is (= (hash empty-db)
             (hash (d/filter empty-db (constantly true)))
             (hash (d/filter database (constantly false)))))))

  (testing "double filtering"
    (let [database
          (d/db-with
           (d/empty-db {})
           [{:db/id 1 :name "Petr" :age 32}
            {:db/id 2 :name "Oleg"}
            {:db/id 3 :name "Ivan" :age 12}])
          has-age?
          (fn [database datom]
            (some? (:age (d/entity database (:e datom)))))
          adult?
          (fn [database datom]
            (>= (:age (d/entity database (:e datom))) 18))
          names (fn [database] (map :v (d/datoms database :aevt :name)))]
      (is (= ["Petr" "Oleg" "Ivan"] (names database)))
      (is (= ["Petr" "Ivan"]
             (names (d/filter database has-age?))))
      (is (= ["Petr"]
             (names (-> database
                        (d/filter has-age?)
                        (d/filter adult?))))))))
