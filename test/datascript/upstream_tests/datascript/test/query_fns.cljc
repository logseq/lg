(ns datascript.test.query-fns
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.lg.query-types :as query-types]
    [datascript.test.core :as tdc]))

(defn ^:int query-int [^query-types/result result]
  (match result
    (Datascript_runtime.Query_value.Entity value) value
    (Datascript_runtime.Query_value.Value
     (Datascript_runtime.Data_value.Int value))
    value
    _ (Stdlib.invalid_arg "Expected an integer query argument")))

(defn ^datascript.db/database-view query-database
  [^query-types/result result]
  (match result
    (Datascript_runtime.Query_value.Database database) database
    _ (Stdlib.invalid_arg "Expected a database query argument")))

(defn ^:option<Datascript_runtime.Data_value.t> adult?
  [^:vector<query-types/result> arguments]
  (if-some [age (first arguments)]
    (Some
     (Datascript_runtime.Data_value.Bool
      (> (query-int age) 18)))
    None))

(defn ^:option<Datascript_runtime.Data_value.t> keep-even
  [^:vector<query-types/result> arguments]
  (if-some [value (first arguments)]
    (let [value (query-int value)]
      (if (even? value)
        (Some (Datascript_runtime.Data_value.Int value))
        None))
    None))

(defn ^:option<Datascript_runtime.Data_value.t> entity-age?
  [^:vector<query-types/result> arguments]
  (if-some [database-result (first arguments)]
    (if-some [entity-result (first (subvec arguments 1))]
      (if-some [age-result (first (subvec arguments 2))]
        (let [database (query-database database-result)
              entity (query-int entity-result)
              age (query-int age-result)
              datoms
              (db/database-view-search-vector
               database (Some entity) (Some :age) None None)]
          (Some
           (Datascript_runtime.Data_value.Bool
            (if-some [datom (first datoms)]
              (Datascript_runtime.Data_value.equal
               (.-v datom)
               (Datascript_runtime.Data_value.Int age))
              false))))
        None)
      None)
    None))

(defn scalar-output-value?
  [^query-types/output output
   ^:Datascript_runtime.Data_value.t expected]
  (match (query-types/output-scalar output)
    (Some (Some result))
    (Datascript_runtime.Data_value.equal
     (tdc/query-result-value result)
     expected)
    _ false))

(defn scalar-output-nan?
  [^query-types/output output]
  (match (query-types/output-scalar output)
    (Some
     (Some
      (Datascript_runtime.Query_value.Value
       (Datascript_runtime.Data_value.Float value))))
    (Float.is_nan value)
    _ false))

(defn scalar-output-missing?
  [^query-types/output output]
  (match (query-types/output-scalar output)
    (Some None) true
    _ false))

(deftest test-core-numeric-query-functions
  (testing "division preserves upstream arities and floating-point results"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(/ 8 2 2) ?x]])
      (Datascript_runtime.Data_value.Float 2.0)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(/ 4) ?x]])
      (Datascript_runtime.Data_value.Float 0.25)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(/ 1 0) ?x]])
      (Datascript_runtime.Data_value.Float ##Inf)))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(/) ?x]]))))

  (testing "quotient, remainder, and modulo retain negative-number semantics"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(quot -7 3) ?x]])
      (Datascript_runtime.Data_value.Int -2)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(rem -7 3) ?x]])
      (Datascript_runtime.Data_value.Int -1)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(mod -7 3) ?x]])
      (Datascript_runtime.Data_value.Int 2)))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(quot 1 0) ?x]])))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(rem 1 0) ?x]])))
    (is
     (scalar-output-nan?
      (d/q '[:find ?x .
             :where [(mod 1 0) ?x]]))))

  (testing "minimum and maximum preserve variadic and empty results"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(max 3 9 4) ?x]])
      (Datascript_runtime.Data_value.Int 9)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(min 3 9 4) ?x]])
      (Datascript_runtime.Data_value.Int 3)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(max 3 4.5) ?x]])
      (Datascript_runtime.Data_value.Float 4.5)))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(max) ?x]])))
    (is
     (scalar-output-missing?
      (d/q '[:find ?x .
             :where [(min) ?x]])))))

(deftest test-core-collection-value-query-functions
  (testing "set preserves uniqueness and nil conversion"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(set [1 1 2]) ?x]])
      (Datascript_runtime.Data_value.Set
       (list
        (Datascript_runtime.Data_value.Int 1)
        (Datascript_runtime.Data_value.Int 2)))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(set nil) ?x]])
      (Datascript_runtime.Data_value.Set (list)))))

  (testing "range preserves one, two, and negative-step arities"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(range 4) ?x]])
      (Datascript_runtime.Data_value.List
       (list
        (Datascript_runtime.Data_value.Int 0)
        (Datascript_runtime.Data_value.Int 1)
        (Datascript_runtime.Data_value.Int 2)
        (Datascript_runtime.Data_value.Int 3)))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(range 2 5) ?x]])
      (Datascript_runtime.Data_value.List
       (list
        (Datascript_runtime.Data_value.Int 2)
        (Datascript_runtime.Data_value.Int 3)
        (Datascript_runtime.Data_value.Int 4)))))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(range 5 0 -2) ?x]])
      (Datascript_runtime.Data_value.List
       (list
        (Datascript_runtime.Data_value.Int 5)
        (Datascript_runtime.Data_value.Int 3)
        (Datascript_runtime.Data_value.Int 1))))))

  (testing "str and subs preserve JavaScript boundary behavior"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(str nil :a 1 "x") ?x]])
      (Datascript_runtime.Data_value.String ":a1x")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(str) ?x]])
      (Datascript_runtime.Data_value.String "")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(subs "hello" 2) ?x]])
      (Datascript_runtime.Data_value.String "llo")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(subs "hello" 1 4) ?x]])
      (Datascript_runtime.Data_value.String "ell")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(subs "hello" -2) ?x]])
      (Datascript_runtime.Data_value.String "hello")))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(subs "hello" 2 20) ?x]])
      (Datascript_runtime.Data_value.String "llo"))))

  (testing "three-argument get returns present values or its default"
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(get {:a 1} :a 9) ?x]])
      (Datascript_runtime.Data_value.Int 1)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(get {:a 1} :b 9) ?x]])
      (Datascript_runtime.Data_value.Int 9)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(get [10] 2 9) ?x]])
      (Datascript_runtime.Data_value.Int 9)))
    (is
     (scalar-output-value?
      (d/q '[:find ?x .
             :where [(get nil :a 9) ?x]])
      (Datascript_runtime.Data_value.Int 9)))))

(deftest test-query-fns
  (testing "predicate without free variables"
    (is (tdc/query-relation?
         (d/q '[:find ?x
                :in [?x ...]
                :where [(> 2 1)]] [:a :b :c])
         [[:a] [:b] [:c]])))

  (let [db (-> (d/empty-db {:parent {:db/valueType :db.type/ref}})
             (d/db-with [{:db/id 1, :name  "Ivan",  :age   15}
                         {:db/id 2, :name  "Petr",  :age   22, :height 240, :parent 1}
                         {:db/id 3, :name  "Slava", :age   37, :parent 2}]))]

    (testing "ground"
      (is (tdc/query-relation?
           (d/q '[:find ?vowel
                  :where [(ground [:a :e :i :o :u]) [?vowel ...]]])
           [[:a] [:e] [:i] [:o] [:u]])))

    (testing "get-else"
      (is (tdc/query-relation?
           (d/q '[:find ?e ?age ?height
                  :where [?e :age ?age]
                  [(get-else $ ?e :height 300) ?height]] db)
           [[1 15 300] [2 22 240] [3 37 300]]))
      
      (is (thrown-msg? "get-else: nil default value is not supported"
            (d/q '[:find ?e ?height
                   :where [?e :age]
                   [(get-else $ ?e :height nil) ?height]] db))))

    (testing "get-some"
      (is (tdc/query-relation?
           (d/q '[:find ?e ?a ?v
                  :where [?e :name _]
                  [(get-some $ ?e :height :age) [?a ?v]]] db)
           [[1 :age 15]
            [2 :height 240]
            [3 :age 37]])))

    (testing "missing?"
      (is (tdc/query-relation?
           (d/q '[:find ?e ?age
                  :in $
                  :where [?e :age ?age]
                  [(missing? $ ?e :height)]] db)
           [[1 15] [3 37]])))

    (testing "missing? back-ref"
      (is (tdc/query-relation?
           (d/q '[:find ?e
                  :in $
                  :where [?e :age ?age]
                  [(missing? $ ?e :_parent)]] db)
           [[3]])))

    (testing "Built-ins"
      (is (tdc/query-relation?
           (d/q '[:find  ?e1 ?e2
                  :where [?e1 :age ?a1]
                  [?e2 :age ?a2]
                  [(< ?a1 18 ?a2)]] db)
           [[1 2] [1 3]]))
      (is (tdc/query-relation?
           (d/q '[:find  ?a1
                  :where [_ :age ?a1]
                  [(< ?a1 22)]] db)
           [[15]]))
      (is (tdc/query-relation?
           (d/q '[:find  ?a1
                  :where [_ :age ?a1]
                  [(<= ?a1 22)]] db)
           [[15] [22]]))
      (is (tdc/query-relation?
           (d/q '[:find  ?a1
                  :where [_ :age ?a1]
                  [(> ?a1 22)]] db)
           [[37]]))
      (is (tdc/query-relation?
           (d/q '[:find  ?a1
                  :where [_ :age ?a1]
                  [(>= ?a1 22)]] db)
           [[22] [37]]))
      (testing "compare values of different types"
        (is (tdc/query-relation?
             (d/q '[:find  ?e
                    :where [?e]
                    [(< ?e 1)]] [[0] [1] [""]])
             [[0]]))
        (is (tdc/query-relation?
             (d/q '[:find  ?e
                    :where [?e]
                    [(<= ?e 1)]] [[0] [1] [""]])
             [[0] [1]]))
        (is (tdc/query-relation?
             (d/q '[:find  ?e
                    :where [?e]
                    [(> ?e 1)]] [[0] [1] [""]])
             [[""]]))
        (is (tdc/query-relation?
             (d/q '[:find  ?e
                    :where [?e]
                    [(>= ?e 1)]] [[0] [1] [""]])
             [[1] [""]])))
      
      (is (tdc/query-relation?
           (d/q '[:find  ?x ?c
                  :in    [?x ...]
                  :where [(count ?x) ?c]]
                ["a" "abc"])
           [["a" 1] ["abc" 3]])))

    (testing "Built-in vector, hashmap"
      (is (tdc/query-collection?
           (d/q '[:find [?tx-data ...]
                  :where
                  [(ground :db/add) ?op]
                  [(vector ?op -1 :attr 12) ?tx-data]])
           [[:db/add -1 :attr 12]]))

      (is (tdc/query-collection?
           (d/q '[:find [?tx-data ...]
                  :where
                  [(hash-map :db/id -1 :age 92 :name "Aaron") ?tx-data]])
           [{:db/id -1 :age 92 :name "Aaron"}])))

    (testing "Passing predicate as source"
      (is (tdc/query-relation?
           (d/q '[:find  ?e
                  :in    $ ?adult
                  :where [?e :age ?a]
                  [(?adult ?a)]]
                db adult?)
           [[2] [3]])))

    (testing "Calling a function"
      (is (tdc/query-relation?
           (d/q '[:find  ?e1 ?e2 ?e3
                  :where [?e1 :age ?a1]
                  [?e2 :age ?a2]
                  [?e3 :age ?a3]
                  [(+ ?a1 ?a2) ?a12]
                  [(= ?a12 ?a3)]]
                db)
           [[1 2 3] [2 1 3]])))

    (testing "Two conflicting function values for one binding."
      (is (tdc/query-relation?
           (d/q '[:find  ?n
                  :where [(identity 1) ?n]
                  [(identity 2) ?n]])
           [])))

    (testing "Destructured conflicting function values for two bindings."
      (is (tdc/query-relation?
           (d/q '[:find  ?n ?x
                  :where [(identity [3 4]) [?n ?x]]
                  [(identity [1 2]) [?n ?x]]])
           [])))

    (testing "Rule bindings interacting with function binding. (fn, rule)"
      (is (tdc/query-relation?
           (d/q '[:find  ?n
                  :in $ %
                  :where [(identity 2) ?n]
                  (my-vals ?n)]
                db
                '[[(my-vals ?x)
                   [(identity 1) ?x]]
                  [(my-vals ?x)
                   [(identity 2) ?x]]
                  [(my-vals ?x)
                   [(identity 3) ?x]]])
           [[2]])))

    (testing "Rule bindings interacting with function binding. (rule, fn)"
      (is (tdc/query-relation?
           (d/q '[:find  ?n
                  :in $ %
                  :where (my-vals ?n)
                  [(identity 2) ?n]]
                db
                '[[(my-vals ?x)
                   [(identity 1) ?x]]
                  [(my-vals ?x)
                   [(identity 2) ?x]]
                  [(my-vals ?x)
                   [(identity 3) ?x]]])
           [[2]])))

    (testing "Conflicting relational bindings with function binding. (rel, fn)"
      (is (tdc/query-relation?
           (d/q '[:find  ?age
                  :where [_ :age ?age]
                  [(identity 100) ?age]]
                db)
           [])))

    (testing "Conflicting relational bindings with function binding. (fn, rel)"
      (is (tdc/query-relation?
           (d/q '[:find  ?age
                  :where [(identity 100) ?age]
                  [_ :age ?age]]
                db)
           [])))

    (testing "Function on empty rel"
      (is (tdc/query-relation?
           (d/q '[:find  ?e ?y
                  :where [?e :salary ?x]
                  [(+ ?x 100) ?y]]
                [[0 :age 15] [1 :age 35]])
           [])))
    
    (testing "Returning nil from function filters out tuple from result"
      (is (tdc/query-relation?
           (d/q '[:find ?x
                  :in    [?in ...] ?f
                  :where [(?f ?in) ?x]]
                [1 2 3 4]
                keep-even)
           [[2] [4]])))

    (testing "Result bindings"
      (is (tdc/query-relation?
           (d/q '[:find ?a ?c
                  :in ?in
                  :where [(ground ?in) [?a _ ?c]]]
                [:a :b :c])
           [[:a :c]]))

      (is (tdc/query-relation?
           (d/q '[:find ?in
                  :in ?in
                  :where [(ground ?in) _]]
                :a)
           [[:a]]))

      (is (tdc/query-relation?
           (d/q '[:find ?x ?z
                  :in ?in
                  :where [(ground ?in) [[?x _ ?z]...]]]
                [[:a :b :c] [:d :e :f]])
           [[:a :c] [:d :f]]))
      
      (is (tdc/query-relation?
           (d/q '[:find ?in
                  :in [?in ...]
                  :where [(ground ?in) _]]
                [])
           [])))))

(deftest test-predicates
  (let [db
        (d/db-with
         (d/empty-db)
         [{:db/id 1 :name "Ivan" :age 10}
          {:db/id 2 :name "Ivan" :age 20}
          {:db/id 3 :name "Oleg" :age 10}
          {:db/id 4 :name "Oleg" :age 20}])]
    (is (tdc/query-relation?
         (d/q '[:find  ?e ?a
                :where [?e :age ?a]
                [(> ?a 10)]] db)
         [[2 20] [4 20]]))

    (is (tdc/query-relation?
         (d/q '[:find  ?e ?e2
                :where [?e  :name]
                [?e2 :name]
                [(< ?e ?e2)]] db)
         [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]))

    (is (tdc/query-relation?
         (d/q '[:find  ?e ?e2
                :where [?e  :age ?a]
                [?e2 :age ?a2]
                [(< ?e ?e2)]] db)
         [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]))

    (is (tdc/query-relation?
         (d/q '[:find  ?e ?e2
                :where [?e  :name "Ivan"]
                [?e2 :name "Oleg"]
                [(= ?e ?e2)]] db)
         []))

    (is (tdc/query-relation?
         (d/q '[:find  ?e
                :where [?e :name "Ivan"]
                [?e :age 20]
                [(= ?e 2)]] db)
         [[2]]))

    (is (tdc/query-relation?
         (d/q '[:find  ?e
                :where [?e :name "Ivan"]
                [?e :age 20]
                [(= ?e 1)]] db)
         []))

    (is (tdc/query-relation?
         (d/q '[:find ?e
                :in $ ?pred
                :where [?e :age ?a]
                [(?pred $ ?e 10)]]
              db entity-age?)
         [[1] [3]]))))

(deftest test-exceptions
  (is (thrown-msg? "Unknown predicate 'fun in [(fun ?e)]"
        (d/q '[:find ?e
               :in   [?e ...]
               :where [(fun ?e)]]
          [1])))
  
  (is (thrown-msg? "Unknown function 'fun in [(fun ?e) ?x]"
        (d/q '[:find ?e ?x
               :in   [?e ...]
               :where [(fun ?e) ?x]]
          [1])))

  (is (thrown-msg? "Insufficient bindings: #{?x} not bound in [(zero? ?x)]"
        (d/q '[:find ?x
               :where [(zero? ?x)]])))

  (is (thrown-msg? "Insufficient bindings: #{?x} not bound in [(inc ?x) ?y]"
        (d/q '[:find ?x
               :where [(inc ?x) ?y]])))

  (is (thrown-msg? "Where uses unknown source vars: [$2]"
        (d/q '[:find ?x
               :where [?x] [(zero? $2 ?x)]])))

  (is (thrown-msg? "Where uses unknown source vars: [$]"
        (d/q '[:find  ?x
               :in    $2 
               :where [$2 ?x] [(zero? $ ?x)]]))))

(deftest test-issue-180
  (is (tdc/query-relation?
       (d/q '[:find ?e ?a
              :where [_ :pred ?pred]
              [?e :age ?a]
              [(?pred ?a)]]
            (d/db-with (d/empty-db) [[:db/add 1 :age 20]]))
       [])))

(defn ^:option<Datascript_runtime.Data_value.t> sample-query-fn
  [^:vector<query-types/result> arguments]
  (if (empty? arguments)
    (Some (Datascript_runtime.Data_value.Int 42))
    None))

#?(:clj
   (deftest test-symbol-resolution
     (is
      (scalar-output-value?
       (d/q '[:find ?x .
              :where [(datascript.test.query-fns/sample-query-fn) ?x]])
       (Datascript_runtime.Data_value.Int 42)))))

(deftest test-issue-445
  (let [db (-> (d/empty-db {:name {:db/unique :db.unique/identity}})
             (d/db-with [{:db/id 1 :name "Ivan" :age 15}
                         {:db/id 2 :name "Petr" :age 22 :height 240}]))]
    (testing "get-else using lookup ref"
      (is
       (scalar-output-value?
        (d/q '[:find ?height .
               :in $ ?e
               :where [(get-else $ ?e :height "Unknown") ?height]]
             db
             [:name "Ivan"])
        (Datascript_runtime.Data_value.String "Unknown"))))

    (testing "get-some using lookup ref"
      (is (tdc/query-relation?
           (d/q '[:find ?e ?a ?v
                  :in $ ?e
                  :where [(get-some $ ?e :weight :age :height) [?a ?v]]]
                db
                [:name "Petr"])
           [[[:name "Petr"] :age 22]])))))
