(ns datascript.test.query-or
  (:require
   [clojure.test :refer [deftest is]]
   [datascript.core :as d]
   [datascript.test.core :as tdc]))

(def *test-db
  (delay
   (d/db-with
    (d/empty-db)
    [{:db/id 1 :name "Ivan" :age 10}
     {:db/id 2 :name "Ivan" :age 20}
     {:db/id 3 :name "Oleg" :age 10}
     {:db/id 4 :name "Oleg" :age 20}
     {:db/id 5 :name "Ivan" :age 10}
     {:db/id 6 :name "Ivan" :age 20}])))

(deftest test-or
  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           (or [?e :name "Oleg"]
               [?e :age 10])]
         @*test-db)
    [[1] [3] [4] [5]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           (or [?e :name "Oleg"]
               [?e :age 30])]
         @*test-db)
    [[3] [4]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           (or [?e :name "Petr"]
               [?e :age 30])]
         @*test-db)
    []))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           [?e :name "Ivan"]
           (or [?e :name "Oleg"]
               [?e :age 10])]
         @*test-db)
    [[1] [5]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           [?e :age ?a]
           (or
            (and [?e :name "Ivan"]
                 [1 :age ?a])
            (and [?e :name "Oleg"]
                 [2 :age ?a]))]
         @*test-db)
    [[1] [4] [5]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           (or
            (and [?e :name "Ivan"]
                 [1 :age ?a])
            (and [?e :name "Oleg"]
                 [2 :age ?a]))
           [?e :age ?a]]
         @*test-db)
    [[1] [4] [5]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           (or
            (and [?e :name "Ivan"]
                 [1 :age ?a])
            (and [2 :age ?a]
                 [?e :name "Oleg"]))
           [?e :age ?a]]
         @*test-db)
    [[1] [4] [5]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           (or
            (and [?e :age 30]
                 [?e :name ?n])
            (and [?e :age 20]
                 [?e :name ?n]))
           [(ground "Ivan") ?n]]
         @*test-db)
    [[2] [6]])))

(deftest test-or-join
  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           (or-join [?e]
             [?e :name ?n]
             (and [?e :age ?a]
                  [?e :name ?n]))]
         @*test-db)
    [[1] [2] [3] [4] [5] [6]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           [?e :name ?a]
           [?e2 :name ?a]
           (or-join [?e]
             (and [?e :age ?a]
                  [?e2 :age ?a]))]
         @*test-db)
    [[1] [2] [3] [4] [5] [6]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :where
           (or-join [?e ?n]
             (and [?e :age 30]
                  [?e :name ?n])
             (and [?e :age 20]
                  [?e :name ?n]))
           [(ground "Ivan") ?n]]
         @*test-db)
    [[2] [6]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :in $ ?a
           :where
           (or
            [?e :age ?a]
            [?e :name "Oleg"])]
         @*test-db 10)
    [[1] [3] [4] [5]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :in $ ?a
           :where
           (or-join [?e ?a]
             [?e :age ?a]
             [?e :name "Oleg"])]
         @*test-db 10)
    [[1] [3] [4] [5]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?e
           :in $ ?a
           :where
           (or-join [[?a] ?e]
             [?e :age ?a]
             [?e :name "Oleg"])]
         @*test-db 10)
    [[1] [3] [4] [5]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?a ?b ?c
           :in $xs $ys
           :where
           [$xs ?a ?b ?c]
           (or-join [?a]
             [$ys ?a ?b ?d])]
         [[:a1 :b1 :c1]
          [:a2 :b2 :c2]
          [:a3 :b3 :c3]]
         [[:a1 :b1 :d1]
          [:a2 :b2* :d2]
          [:a4 :b4 :c4]])
    [[:a1 :b1 :c1]
     [:a2 :b2 :c2]]))

  (is
   (tdc/query-relation?
    (d/q '[:find ?a ?c
           :in $xs $ys
           :where
           (or-join [?a ?c]
             [$xs ?a ?b ?c]
             [$ys ?a ?c])]
         [[:a1 :b1 :c1]]
         [[:a2 :c2]])
    [[:a1 :c1] [:a2 :c2]])))

(deftest test-default-source
  (let [db1
        (d/db-with
         (d/empty-db)
         [[:db/add 1 :name "Ivan"]
          [:db/add 2 :name "Oleg"]])
        db2
        (d/db-with
         (d/empty-db)
         [[:db/add 1 :age 10]
          [:db/add 2 :age 20]])]
    (is
     (tdc/query-relation?
      (d/q '[:find ?e
             :in $ $2
             :where
             [?e :name]
             (or [?e :name "Ivan"])]
           db1 db2)
      [[1]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?e
             :in $ $2
             :where
             [?e :name]
             (or [$2 ?e :age 10])]
           db1 db2)
      [[1]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?e
             :in $ $2
             :where
             [?e :name]
             ($2 or [?e :age 10])]
           db1 db2)
      [[1]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?e
             :in $ $2
             :where
             [?e :name]
             ($2 or [$ ?e :name "Ivan"])]
           db1 db2)
      [[1]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?e
             :in $ $2
             :where
             [?e :name]
             ($2 or (or [?e :age 10]))]
           db1 db2)
      [[1]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?e
             :in $ $2
             :where
             [?e :name]
             ($2 or ($ or [?e :name "Ivan"]))]
           db1 db2)
      [[1]]))))

(deftest test-const-substitution
  (let [db
        (d/db-with
         (d/empty-db {:parent {:db/valueType :db.type/ref}})
         [{:db/id "Ivan" :name "Ivan"}
          {:db/id "Oleg" :name "Oleg" :parent "Ivan"}
          {:db/id "Petr" :name "Petr" :parent "Oleg"}])]
    (is
     (tdc/query-relation?
      (d/q '[:find ?name ?x ?y
             :in $ ?name
             :where
             [?x :name ?name]
             (or-join [?x ?y]
               (and
                [?x :parent ?z]
                [?z :parent ?y])
               [?y :parent ?x])]
           db "Ivan")
      [["Ivan" 1 2]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?name ?x ?y
             :in $ ?name
             :where
             [?x :name ?name]
             (or-join [?x ?y]
               (and
                [?x :parent ?z]
                [?z :parent ?y])
               [?x :parent ?y])]
           db "Ivan")
      []))))

(deftest test-errors
  (is
   (thrown-msg?
    "All clauses in 'or' must use same set of free vars, had [#{?e} #{?e ?a}] in (or [?e :name _] [?e :age ?a])"
    (d/q '[:find ?e
           :where
           (or [?e :name _]
               [?e :age ?a])]
         @*test-db)))

  (is
   (thrown-msg?
    "Insufficient bindings: #{?e} not bound in (or-join [[?e]] [?e :name \"Ivan\"])"
    (d/q '[:find ?e
           :where
           (or-join [[?e]]
             [?e :name "Ivan"])]
         @*test-db))))
