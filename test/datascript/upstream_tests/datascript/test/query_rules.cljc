(ns datascript.test.query-rules
  (:require
    [clojure.test :as t :refer [is deftest testing]]
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

(defn ^:option<Datascript_runtime.Data_value.t> even-callable
  [^:vector<query-types/result> arguments]
  (if-some [value (first arguments)]
    (Some
     (Datascript_runtime.Data_value.Bool
      (even? (query-int value))))
    None))

(defn ^:option<Datascript_runtime.Data_value.t> always-true-callable
  [^:vector<query-types/result> _arguments]
  (Some (Datascript_runtime.Data_value.Bool true)))

(defn query-rows-equal?
  [^:array<query-types/result> left
   ^:array<query-types/result> right]
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

(defn query-relations-equal?
  [^query-types/output left ^query-types/output right]
  (match (tuple
          (query-types/output-relation left)
          (query-types/output-relation right))
    (tuple (Some left-rows) (Some right-rows))
    (and
     (= (count left-rows) (count right-rows))
     (every?
      (fn [^:array<query-types/result> left-row]
        (some
         (fn [^:array<query-types/result> right-row]
           (query-rows-equal? left-row right-row))
         right-rows))
      left-rows))
    _ false))

(deftest test-rules
  (is
   (tdc/query-relation?
    (d/q '[:find ?e1 ?e2
           :in $ %
           :where (follow ?e1 ?e2)]
         [[5 :follow 3]
          [1 :follow 2]
          [2 :follow 3]
          [3 :follow 4]
          [4 :follow 6]
          [2 :follow 4]]
         '[[(follow ?x ?y)
            [?x :follow ?y]]])
    [[1 2] [2 3] [3 4] [2 4] [5 3] [4 6]]))

  (testing "Joining regular clauses with rule"
    (is
     (tdc/query-relation?
      (d/q '[:find ?y ?x
             :in $ %
             :where [_ _ ?x]
             (rule ?x ?y)
             [(even? ?x)]]
           [[5 :follow 3]
            [1 :follow 2]
            [2 :follow 3]
            [3 :follow 4]
            [4 :follow 6]
            [2 :follow 4]]
           '[[(rule ?a ?b)
              [?a :follow ?b]]])
      [[3 2] [6 4] [4 2]])))

  (testing "Rule context is isolated from outer context"
    (is
     (tdc/query-relation?
      (d/q '[:find ?x
             :in $ %
             :where [?e _ _]
             (rule ?x)]
           [[5 :follow 3]
            [1 :follow 2]
            [2 :follow 3]
            [3 :follow 4]
            [4 :follow 6]
            [2 :follow 4]]
           '[[(rule ?e)
              [_ ?e _]]])
      [[:follow]])))

  (testing "Rule with branches"
    (is
     (tdc/query-relation?
      (d/q '[:find ?e2
             :in $ ?e1 %
             :where (follow ?e1 ?e2)]
           [[5 :follow 3]
            [1 :follow 2]
            [2 :follow 3]
            [3 :follow 4]
            [4 :follow 6]
            [2 :follow 4]]
           1
           '[[(follow ?e2 ?e1)
              [?e2 :follow ?e1]]
             [(follow ?e2 ?e1)
              [?e2 :follow ?t]
              [?t :follow ?e1]]])
      [[2] [3] [4]])))

  (testing "Recursive rules"
    (is
     (tdc/query-relation?
      (d/q '[:find ?e2
             :in $ ?e1 %
             :where (follow ?e1 ?e2)]
           [[5 :follow 3]
            [1 :follow 2]
            [2 :follow 3]
            [3 :follow 4]
            [4 :follow 6]
            [2 :follow 4]]
           1
           '[[(follow ?e1 ?e2)
              [?e1 :follow ?e2]]
             [(follow ?e1 ?e2)
              [?e1 :follow ?t]
              (follow ?t ?e2)]])
      [[2] [3] [4] [6]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?e1 ?e2
             :in $ %
             :where (follow ?e1 ?e2)]
           [[1 :follow 2] [2 :follow 3]]
           '[[(follow ?e1 ?e2)
              [?e1 :follow ?e2]]
             [(follow ?e1 ?e2)
              (follow ?e2 ?e1)]])
      [[1 2] [2 3] [2 1] [3 2]]))

    (is
     (tdc/query-relation?
      (d/q '[:find ?e1 ?e2
             :in $ %
             :where (follow ?e1 ?e2)]
           [[1 :follow 2] [2 :follow 3] [3 :follow 1]]
           '[[(follow ?e1 ?e2)
              [?e1 :follow ?e2]]
             [(follow ?e1 ?e2)
              (follow ?e2 ?e1)]])
      [[1 2] [2 3] [3 1] [2 1] [3 2] [1 3]])))

  (testing "Mutually recursive rules"
    (is
     (tdc/query-relation?
      (d/q '[:find ?e1 ?e2
             :in $ %
             :where (f1 ?e1 ?e2)]
           [[0 :f1 1]
            [1 :f2 2]
            [2 :f1 3]
            [3 :f2 4]
            [4 :f1 5]
            [5 :f2 6]]
           '[[(f1 ?e1 ?e2)
              [?e1 :f1 ?e2]]
             [(f1 ?e1 ?e2)
              [?t :f1 ?e2]
              (f2 ?e1 ?t)]
             [(f2 ?e1 ?e2)
              [?e1 :f2 ?e2]]
             [(f2 ?e1 ?e2)
              [?t :f2 ?e2]
              (f1 ?e1 ?t)]])
      [[0 1] [0 3] [0 5]
       [1 3] [1 5]
       [2 3] [2 5]
       [3 5]
       [4 5]])))

  (testing "Passing ins to rule"
    (is
     (tdc/query-relation?
      (d/q '[:find ?x ?y
             :in $ % ?even
             :where (match ?even ?x ?y)]
           [[5 :follow 3]
            [1 :follow 2]
            [2 :follow 3]
            [3 :follow 4]
            [4 :follow 6]
            [2 :follow 4]]
           '[[(match ?pred ?e ?e2)
              [?e :follow ?e2]
              [(?pred ?e)]
              [(?pred ?e2)]]]
           even-callable)
      [[4 6] [2 4]])))

  (testing "Using built-ins inside rule"
    (is
     (tdc/query-relation?
      (d/q '[:find ?x ?y
             :in $ %
             :where (match ?x ?y)]
           [[5 :follow 3]
            [1 :follow 2]
            [2 :follow 3]
            [3 :follow 4]
            [4 :follow 6]
            [2 :follow 4]]
           '[[(match ?e ?e2)
              [?e :follow ?e2]
              [(even? ?e)]
              [(even? ?e2)]]])
      [[4 6] [2 4]])))

  (testing "Calling rule twice (issue-44)"
    (is
     (tdc/query-relation?
      (d/q '[:find ?p
             :in $ % ?fn
             :where (rule ?p ?fn "a")
             (rule ?p ?fn "b")]
           [[1 :attr "a"]]
           '[[(rule ?p ?fn ?x)
              [?p :attr ?x]
              [(?fn ?x)]]]
           always-true-callable)
      [])))

  (testing "Specifying db to rule"
    (is
     (tdc/query-relation?
      (d/q '[:find ?n
             :in $sexes $ages %
             :where ($sexes male ?n)
             ($ages adult ?n)]
           [["Ivan" :male]
            ["Darya" :female]
            ["Oleg" :male]
            ["Igor" :male]]
           [["Ivan" 15]
            ["Oleg" 66]
            ["Darya" 32]]
           '[[(male ?x)
              [?x :male]]
             [(adult ?y)
              [?y ?a]
              [(>= ?a 18)]]])
      [["Oleg"]])))

  (testing "Rule name validation issue-319"
    (is
     (thrown-msg? "Unknown rule 'wat in (wat ?x)"
       (d/q '[:find ?x
              :in $ %
              :where (wat ?x)]
            [] []))))

  (testing "Rule vars validation"
    (is
     (thrown-msg?
      "Cannot parse var, expected symbol starting with ?, got: $e1"
      (d/q '[:find ?e
             :in $ %
             :where [?e]]
           (d/empty-db)
           '[[(rule $e1 ?e2)
              [?e1 :ref ?e2]]])))))

(deftest test-false-arguments
  (let [database
        (d/db-with
         (d/empty-db)
         [(db/tx-add
           (Datascript_runtime.Data_value.Entity_id 1)
           :attr
           (Datascript_runtime.Data_value.Bool true))
          (db/tx-add
           (Datascript_runtime.Data_value.Entity_id 2)
           :attr
           (Datascript_runtime.Data_value.Bool false))])]
    (is
     (tdc/query-relation?
      (d/q '[:find ?id
             :in $ %
             :where (is ?id true)]
           database
           '[[(is ?id ?val)
              [?id :attr ?val]]])
      [[1]]))
    (is
     (tdc/query-relation?
      (d/q '[:find ?id
             :in $ %
             :where (is ?id false)]
           database
           '[[(is ?id ?val)
              [?id :attr ?val]]])
      [[2]]))))

(defn ^:float now-ms []
  #?(:clj (* (Unix/gettimeofday) 1000.0)
     :cljs (js/performance.now)))

(defn ^:vector<datascript.db/tx-entry> performance-transactions []
  (loop [x 1
         transactions []]
    (if (< x 50000)
      (let [entity
            (Datascript_runtime.Data_value.Entity_id (- x))
            status
            (if (= (mod x 3) 0)
              "started"
              (if (= (mod x 3) 1)
                "pending"
                "stopped"))]
        (recur
         (inc x)
         (conj
          (conj
           transactions
           (db/tx-add
            entity
            :item/id
            (Datascript_runtime.Data_value.Int x)))
          (db/tx-add
           entity
           :item/status
           (Datascript_runtime.Data_value.String status)))))
      transactions)))

(deftest test-rule-performance-on-larger-datasets
  (let [database
        (d/db-with (d/empty-db) (performance-transactions))
        inline-start (now-ms)
        inline-result
        (d/q '[:find ?e
               :where [?e :item/status ?status]
               [(ground "pending") ?status]]
             database)
        inline-time (- (now-ms) inline-start)
        rule-start (now-ms)
        rule-result
        (d/q '[:find ?e
               :in $ %
               :where [?e :item/status ?status]
               (pending? ?status)]
             database
             '[[(pending? ?status)
                [(ground "pending") ?status]]])
        rule-time (- (now-ms) rule-start)]
    (is (query-relations-equal? inline-result rule-result))
    (is (<= 0.0 rule-time (* 10.0 inline-time)))))
