(ns datascript.test.query-not
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.test.core :as tdc]))

(def *test-db
  (delay
    (d/db-with (d/empty-db)
      [{:db/id 1 :name "Ivan" :age 10}
       {:db/id 2 :name "Ivan" :age 20}
       {:db/id 3 :name "Oleg" :age 10}
       {:db/id 4 :name "Oleg" :age 20}
       {:db/id 5 :name "Ivan" :age 10}
       {:db/id 6 :name "Ivan" :age 20}])))

(deftest test-not
  (is (tdc/query-collection?
       (d/q '[:find [?e ...]
              :where [?e :name]
              (not [?e :name "Ivan"])]
            @*test-db)
       [3 4]))

  (is (tdc/query-collection?
       (d/q '[:find [?e ...]
              :where [?e :name]
              (not
               [?e :name "Ivan"]
               [?e :age 10])]
            @*test-db)
       [2 3 4 6]))

  (is (tdc/query-collection?
       (d/q '[:find [?e ...]
              :where [?e :name]
              (not [?e :name "Ivan"])
              (not [?e :age 10])]
            @*test-db)
       [4]))

  (is (tdc/query-collection?
       (d/q '[:find [?e ...]
              :where [?e :name]
              (not [?e :age])]
            @*test-db)
       []))

  (is (tdc/query-collection?
       (d/q '[:find [?e ...]
              :where [?e :name "Ivan"]
              (not [?e :name "Oleg"])]
            @*test-db)
       [1 2 5 6]))

  (is (tdc/query-collection?
       (d/q '[:find [?e ...]
              :where [?e :name]
              (not
               [?e :name "Ivan"]
               [?e :name "Oleg"])]
            @*test-db)
       [1 2 3 4 5 6]))

  (is (tdc/query-collection?
       (d/q '[:find [?e ...]
              :where [?e :name]
              (not
               [?e :name "Ivan"]
               (not [?e :age 10]))]
            @*test-db)
       [1 3 4 5]))

  (is (tdc/query-collection?
       (d/q '[:find [?e ...]
              :where [?e :name ?a]
              (not
               [?e :age ?f]
               [?e :age 10])]
            @*test-db)
       [2 4 6])))

(deftest test-not-join
  (is (tdc/query-relation?
       (d/q '[:find ?e ?a
              :where [?e :name]
              [?e :age ?a]
              (not-join [?e]
                [?e :name "Oleg"]
                [?e :age ?a])]
            @*test-db)
       [[1 10] [2 20] [5 10] [6 20]]))

  (is (tdc/query-relation?
       (d/q '[:find ?e ?a
              :where [?e :age ?a]
              [?e :age 10]
              (not-join [?e]
                [?e :name "Oleg"]
                [?e :age ?a]
                [?e :age 10])]
            @*test-db)
       [[1 10] [5 10]])))
  
(deftest test-default-source
  (let [db1 (d/db-with (d/empty-db)
              [[:db/add 1 :name "Ivan"]
               [:db/add 2 :name "Oleg"]])
        db2 (d/db-with (d/empty-db)
              [[:db/add 1 :age 10]
               [:db/add 2 :age 20]])]
    (is (tdc/query-collection?
         (d/q '[:find [?e ...]
                :in $ $2
                :where [?e :name]
                (not [?e :name "Ivan"])]
              db1 db2)
         [2]))

    (is (tdc/query-collection?
         (d/q '[:find [?e ...]
                :in $ $2
                :where [?e :name]
                (not [$2 ?e :age 10])]
              db1 db2)
         [2]))

    (is (tdc/query-collection?
         (d/q '[:find [?e ...]
                :in $ $2
                :where [?e :name]
                ($2 not [?e :age 10])]
              db1 db2)
         [2]))

    (is (tdc/query-collection?
         (d/q '[:find [?e ...]
                :in $ $2
                :where [?e :name]
                ($2 not [$ ?e :name "Ivan"])]
              db1 db2)
         [2]))

    (is (tdc/query-collection?
         (d/q '[:find [?e ...]
                :in $ $2
                :where [?e :name]
                ($2 not (not [?e :age 10]))]
              db1 db2)
         [1]))

    (is (tdc/query-collection?
         (d/q '[:find [?e ...]
                :in $ $2
                :where [?e :name]
                ($2 not ($ not [?e :name "Ivan"]))]
              db1 db2)
         [1]))))

(deftest test-impl-edge-cases
  (is (tdc/query-relation?
       (d/q '[:find ?e
              :where [?e :name "Oleg"]
              [?e :age 10]
              (not [?e :age 20])]
            @*test-db)
       [[3]]))

  (is (tdc/query-relation?
       (d/q '[:find ?e
              :where [?e :name "Oleg"]
              [?e :age 10]
              (not [?e :age 10])]
            @*test-db)
       []))

  (is (tdc/query-relation?
       (d/q '[:find ?e
              :where [?e :name "Oleg"]
              (not [?e :age 10])]
            @*test-db)
       [[4]]))

  (is (tdc/query-relation?
       (d/q '[:find ?e ?e2
              :where [?e :name "Ivan"]
              [?e2 :name "Ivan"]
              (not
               [?e :age 10]
               [?e2 :age 20])]
            @*test-db)
       [[2 1] [6 5] [1 1] [2 2] [5 5] [6 6]
        [2 5] [1 5] [2 6] [6 1] [5 1] [6 2]]))

  (is (tdc/query-relation?
       (d/q '[:find ?e ?e2
              :where [?e :name "Ivan"]
              [?e2 :name "Oleg"]
              (not
               [?e :age 10]
               [?e2 :age 20])]
            @*test-db)
       [[2 3] [1 3] [2 4] [6 3] [5 3] [6 4]]))

  (is (tdc/query-relation?
       (d/q '[:find ?e ?e2
              :where [?e :name "Oleg"]
              [?e2 :name "Oleg"]
              (not
               [?e :age 10]
               [?e2 :age 20])]
            @*test-db)
       [[4 3] [3 3] [4 4]])))

(deftest test-insufficient-bindings
  (is
   (thrown-msg?
    "Insufficient bindings: none of #{?e} is bound in (not [?e :name \"Ivan\"])"
    (d/q '[:find ?e
           :where (not [?e :name "Ivan"])
           [?e :name]]
         @*test-db)))

  (is
   (thrown-msg?
    "Insufficient bindings: none of #{?a} is bound in (not [1 :age ?a])"
    (d/q '[:find ?e
           :where [?e :name]
           (not-join [?e]
             (not [1 :age ?a])
             [?e :age ?a])]
         @*test-db)))

  (is
   (thrown-msg?
    "Insufficient bindings: none of #{?a} is bound in (not [?a :name \"Ivan\"])"
    (d/q '[:find ?e
           :where [?e :name]
           (not [?a :name "Ivan"])]
         @*test-db))))
