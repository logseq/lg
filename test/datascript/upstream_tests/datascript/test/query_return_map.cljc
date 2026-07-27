(ns datascript.test.query-return-map
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

(defn query-result-map
  [^:string name-key ^:string age-key ^:string name ^:int age]
  (zipmap
   [name-key age-key]
   [(query-string name) (query-int age)]))

(defn result-map-equal?
  [^:map<string;datascript.lg.query-types/result> left
   ^:map<string;datascript.lg.query-types/result> right]
  (and
   (= (count left) (count right))
   (every?
    (fn [^:string key]
      (if-some [left-value (get left key)]
        (if-some [right-value (get right key)]
          (Datascript_runtime.Query_value.equal_result
           left-value right-value)
          false)
        false))
    (keys left))))

(defn result-map-vector-contains?
  [^:vector<map<string;datascript.lg.query-types/result>> values
   ^:map<string;datascript.lg.query-types/result> expected]
  (if-some [value (first values)]
    (if (result-map-equal? value expected)
      true
      (result-map-vector-contains? (subvec values 1) expected))
    false))

(defn result-map-relation-equal?
  [^:vector<map<string;datascript.lg.query-types/result>> actual
   ^:vector<map<string;datascript.lg.query-types/result>> expected]
  (and
   (= (count actual) (count expected))
   (every?
    (fn [^:map<string;datascript.lg.query-types/result> value]
      (result-map-vector-contains? actual value))
    expected)))

(def *test-db
  (delay
    (d/db-with (d/empty-db)
      [[:db/add 1 :name "Petr"]
       [:db/add 1 :age 44]
       [:db/add 2 :name "Ivan"]
       [:db/add 2 :age 25]
       [:db/add 3 :name "Sergey"]
       [:db/add 3 :age 11]])))

(deftest test-find-specs
  (is (if-some
       [actual
        (query-types/output-keyword-relation
         (d/q '[:find ?name ?age
                :keys n a
                :where [?e :name ?name]
                [?e :age  ?age]]
              @*test-db))]
        (result-map-relation-equal?
         actual
         [(query-result-map ":n" ":a" "Petr" 44)
          (query-result-map ":n" ":a" "Ivan" 25)
          (query-result-map ":n" ":a" "Sergey" 11)])
        false))
  (is (if-some
       [actual
        (query-types/output-symbol-relation
         (d/q '[:find ?name ?age
                :syms n a
                :where [?e :name ?name]
                [?e :age  ?age]]
              @*test-db))]
        (result-map-relation-equal?
         actual
         [(query-result-map "n" "a" "Petr" 44)
          (query-result-map "n" "a" "Ivan" 25)
          (query-result-map "n" "a" "Sergey" 11)])
        false))
  (is (if-some
       [actual
        (query-types/output-string-relation
         (d/q '[:find ?name ?age
                :strs n a
                :where [?e :name ?name]
                [?e :age  ?age]]
              @*test-db))]
        (result-map-relation-equal?
         actual
         [(query-result-map "n" "a" "Petr" 44)
          (query-result-map "n" "a" "Ivan" 25)
          (query-result-map "n" "a" "Sergey" 11)])
        false))

  (is (match
       (query-types/output-keyword-tuple
        (d/q '[:find [?name ?age]
               :keys n a
               :where [?e :name ?name]
               [(= ?name "Ivan")]
               [?e :age  ?age]]
             @*test-db))
       (Some (Some actual))
       (result-map-equal?
        actual
        (query-result-map ":n" ":a" "Ivan" 25))
       _ false)))

