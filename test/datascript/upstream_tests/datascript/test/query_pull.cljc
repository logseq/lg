(ns datascript.test.query-pull
  (:require
   [clojure.test :refer [deftest is]]
   [datascript.core :as d]
   [datascript.lg.query-types :as query-types]
   [datascript.test.core :as tdc]))

(def *test-db
  (delay
   (d/db-with
    (d/empty-db)
    [{:db/id 1 :name "Petr" :age 44}
     {:db/id 2 :name "Ivan" :age 25}
     {:db/id 3 :name "Oleg" :age 11}])))

(defn ^:Datascript_runtime.Data_value.t pull-name
  [^:string name]
  (Datascript_runtime.Data_value.map_of_keyword_map
   {:name (Datascript_runtime.Data_value.String name)}))

(defn ^boolean query-scalar-value?
  [^datascript.lg.query-types/output output
   ^:Datascript_runtime.Data_value.t expected]
  (if-some [actual (query-types/output-scalar output)]
    (if-some [result actual]
      (Datascript_runtime.Data_value.equal
       (tdc/query-result-value result)
       expected)
      false)
    false))

(defn ^boolean query-tuple-values?
  [^datascript.lg.query-types/output output
   ^:vector<Datascript_runtime.Data_value.t> expected]
  (if-some [actual (query-types/output-tuple output)]
    (if-some [row actual]
      (tdc/query-result-row-equal? row (to-array expected))
      false)
    false))

(deftest test-basics
  (is
   (tdc/query-relation?
    (d/q '[:find (pull ?e [:name])
           :where
           [?e :age ?a]
           [(>= ?a 18)]]
         @*test-db)
    [[{:name "Ivan"}]
     [{:name "Petr"}]]))

  (is
   (tdc/query-relation?
    (d/q '[:find (pull ?e [*])
           :where
           [?e :age ?a]
           [(>= ?a 18)]]
         @*test-db)
    [[{:db/id 2 :age 25 :name "Ivan"}]
     [{:db/id 1 :age 44 :name "Petr"}]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e (pull ?e [:name])
           :where
           [?e :age ?a]
           [(>= ?a 18)]]
         @*test-db)
    [[2 {:name "Ivan"}]
     [1 {:name "Petr"}]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e ?a (pull ?e [:name])
           :where
           [?e :age ?a]
           [(>= ?a 18)]]
         @*test-db)
    [[2 25 {:name "Ivan"}]
     [1 44 {:name "Petr"}]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e (pull ?e [:name]) ?a
           :where
           [?e :age ?a]
           [(>= ?a 18)]]
         @*test-db)
    [[2 {:name "Ivan"} 25]
     [1 {:name "Petr"} 44]])))

(deftest test-var-pattern
  (is
   (tdc/query-relation?
    (d/q '[:find (pull ?e ?pattern)
           :in $ ?pattern
           :where
           [?e :age ?a]
           [(>= ?a 18)]]
         @*test-db [:name])
    [[{:name "Ivan"}]
     [{:name "Petr"}]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e ?a ?pattern (pull ?e ?pattern)
           :in $ ?pattern
           :where
           [?e :age ?a]
           [(>= ?a 18)]]
         @*test-db [:name])
    [[2 25 [:name] {:name "Ivan"}]
     [1 44 [:name] {:name "Petr"}]])))

(deftest test-multiple-sources
  (let [db1
        (d/db-with
         (d/empty-db)
         [{:db/id 1 :name "Ivan" :age 25}])
        db2
        (d/db-with
         (d/empty-db)
         [{:db/id 1 :name "Petr" :age 25}])]
    (is
     (tdc/query-relation?
      (d/q '[:find ?e (pull $1 ?e [:name])
             :in $1 $2
             :where
             [$1 ?e :age 25]]
           db1 db2)
      [[1 {:name "Ivan"}]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?e (pull $2 ?e [:name])
             :in $1 $2
             :where
             [$2 ?e :age 25]]
           db1 db2)
      [[1 {:name "Petr"}]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?e (pull ?e [:name])
             :in $1 $
             :where
             [$ ?e :age 25]]
           db1 db2)
      [[1 {:name "Petr"}]]))))

(deftest test-find-spec
  (is
   (query-scalar-value?
    (d/q '[:find (pull ?e [:name]) .
           :where
           [?e :age 25]]
         @*test-db)
    (pull-name "Ivan")))

  (is
   (tdc/query-collection?
    (d/q '[:find [(pull ?e [:name]) ...]
           :where
           [?e :age ?a]]
         @*test-db)
    [{:name "Ivan"}
     {:name "Petr"}
     {:name "Oleg"}]))

  (is
   (query-tuple-values?
    (d/q '[:find [?e (pull ?e [:name])]
           :where
           [?e :age 25]]
         @*test-db)
    [(Datascript_runtime.Data_value.Int 2)
     (pull-name "Ivan")])))

(deftest test-find-spec-input
  (is
   (query-scalar-value?
    (d/q '[:find (pull ?e ?p) .
           :in $ ?p
           :where
           [(ground 2) ?e]]
         @*test-db [:name])
    (pull-name "Ivan")))

  (is
   (query-scalar-value?
    (d/q '[:find (pull ?e p) .
           :in $ p
           :where
           [(ground 2) ?e]]
         @*test-db [:name])
    (pull-name "Ivan"))))

(deftest test-aggregates
  (let [db
        (d/db-with
         (d/empty-db
          {:value {:db/cardinality :db.cardinality/many}})
         [{:db/id 1 :name "Petr" :value [10 20 30 40]}
          {:db/id 2 :name "Ivan" :value [14 16]}
          {:db/id 3 :name "Oleg" :value 1}])]
    (is
     (tdc/query-relation?
      (d/q '[:find ?e (pull ?e [:name]) (min ?v) (max ?v)
             :where
             [?e :value ?v]]
           db)
      [[1 {:name "Petr"} 10 40]
       [2 {:name "Ivan"} 14 16]
       [3 {:name "Oleg"} 1 1]]))))

(deftest test-lookup-refs
  (let [db
        (d/db-with
         (d/empty-db
          {:name {:db/unique :db.unique/identity}})
         [{:db/id 1 :name "Petr" :age 44}
          {:db/id 2 :name "Ivan" :age 25}
          {:db/id 3 :name "Oleg" :age 11}])
        output
        (d/q '[:find ?ref ?a (pull ?ref [:db/id :name])
               :in $ [?ref ...]
               :where
               [?ref :age ?a]
               [(>= ?a 18)]]
             db
             [[:name "Ivan"]
              [:name "Oleg"]
              [:name "Petr"]])]
    (is
     (tdc/query-relation?
      output
      [[[:name "Petr"] 44 {:db/id 1 :name "Petr"}]
       [[:name "Ivan"] 25 {:db/id 2 :name "Ivan"}]]))))
