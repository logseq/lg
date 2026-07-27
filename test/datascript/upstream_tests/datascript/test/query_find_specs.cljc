(ns datascript.test.query-find-specs
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.lg.query-types :as query-types]
    [datascript.test.core :as tdc]))

(defn query-string [^:string value]
  (query-types/value-result
   (Datascript_runtime.Data_value.String value)))

(defn query-int [^:int value]
  (query-types/value-result
   (Datascript_runtime.Data_value.Int value)))

(defn result-vector-contains?
  [^:vector<datascript.lg.query-types/result> values
   ^datascript.lg.query-types/result expected]
  (if-some [value (first values)]
    (if (Datascript_runtime.Query_value.equal_result value expected)
      true
      (result-vector-contains? (subvec values 1) expected))
    false))

(defn collection-output-equal?
  [^datascript.lg.query-types/output output
   ^:vector<datascript.lg.query-types/result> expected]
  (if-some [values (query-types/output-collection output)]
    (and
     (= (count values) (count expected))
     (every?
      (fn [^datascript.lg.query-types/result value]
        (result-vector-contains? values value))
      expected))
    false))

(defn query-rows-equal?
  [^:array<datascript.lg.query-types/result> left
   ^:array<datascript.lg.query-types/result> right]
  (if (= (alength left) (alength right))
    (loop [index 0]
      (if (< index (alength left))
        (if
         (Datascript_runtime.Query_value.equal_result
          (aget left index)
          (aget right index))
          (recur (inc index))
          false)
        true))
    false))

(defn tuple-output-equal?
  [^datascript.lg.query-types/output output
   ^:array<datascript.lg.query-types/result> expected]
  (match (query-types/output-tuple output)
    (Some (Some actual)) (query-rows-equal? actual expected)
    _ false))

(defn scalar-output-equal?
  [^datascript.lg.query-types/output output
   ^datascript.lg.query-types/result expected]
  (match (query-types/output-scalar output)
    (Some (Some actual))
    (Datascript_runtime.Query_value.equal_result actual expected)
    _ false))

(defn tuple-vector-contains?
  [^:vector<array<datascript.lg.query-types/result>> values
   ^:array<datascript.lg.query-types/result> expected]
  (if-some [value (first values)]
    (if (query-rows-equal? value expected)
      true
      (tuple-vector-contains? (subvec values 1) expected))
    false))

(def *test-db
  (delay
    (d/db-with
      (d/empty-db)
      [[:db/add 1 :name "Petr"]
       [:db/add 1 :age 44]
       [:db/add 2 :name "Ivan"]
       [:db/add 2 :age 25]
       [:db/add 3 :name "Sergey"]
       [:db/add 3 :age 11]])))

(deftest test-find-specs
  (is (collection-output-equal?
       (d/q '[:find [?name ...]
              :where [_ :name ?name]] @*test-db)
       [(query-string "Ivan")
        (query-string "Petr")
        (query-string "Sergey")]))
  (is (tuple-output-equal?
       (d/q '[:find [?name ?age]
              :where [1 :name ?name]
              [1 :age  ?age]] @*test-db)
       (array (query-string "Petr") (query-int 44))))
  (is (scalar-output-equal?
       (d/q '[:find ?name .
              :where [1 :name ?name]] @*test-db)
       (query-string "Petr")))

  (testing "Multiple results get cut"
    (is (if-some
         [tuple
          (match
           (query-types/output-tuple
            (d/q '[:find [?name ?age]
                   :where [?e :name ?name]
                   [?e :age  ?age]] @*test-db))
           (Some tuple) tuple
           _ None)]
          (tuple-vector-contains?
           [(array (query-string "Petr") (query-int 44))
            (array (query-string "Ivan") (query-int 25))
            (array (query-string "Sergey") (query-int 11))]
           tuple)
          false))
    (is (if-some
         [value
          (match
           (query-types/output-scalar
            (d/q '[:find ?name .
                   :where [_ :name ?name]] @*test-db))
           (Some value) value
           _ None)]
          (result-vector-contains?
           [(query-string "Ivan")
            (query-string "Petr")
            (query-string "Sergey")]
           value)
          false)))

  (testing "Aggregates work with find specs"
    (is (collection-output-equal?
         (d/q '[:find [(count ?name) ...]
                :where [_ :name ?name]] @*test-db)
         [(query-int 3)]))
    (is (tuple-output-equal?
         (d/q '[:find [(count ?name)]
                :where [_ :name ?name]] @*test-db)
         (array (query-int 3))))
    (is (scalar-output-equal?
         (d/q '[:find (count ?name) .
                :where [_ :name ?name]] @*test-db)
         (query-int 3)))))
