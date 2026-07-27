(ns datascript.test.query-aggregates
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.lg.query-types :as query-types]
    [datascript.test.core :as tdc]))

(defn ^:int query-int [^query-types/result result]
  (match result
    (Datascript_runtime.Query_value.Value
     (Datascript_runtime.Data_value.Int value))
    value
    _ (Stdlib.invalid_arg "Expected an integer aggregate argument")))

(defn ^:option<Datascript_runtime.Data_value.t> sort-reverse
  [^:vector<query-types/result> arguments]
  (Some
   (Datascript_runtime.Data_value.vector_of_vector
    (mapv
     (fn [^:int value]
       (Datascript_runtime.Data_value.Int value))
     (reverse (sort (mapv query-int arguments)))))))

(deftest test-aggregates
  (testing "with"
    (is (tdc/query-relation?
         (d/q '[:find ?heads
                :with ?monster
                :in   [[?monster ?heads]]]
              [["Medusa" 1]
               ["Cyclops" 1]
               ["Chimera" 1]])
         [[1] [1] [1]])))

  (testing "Wrong grouping without :with"
    (is (tdc/query-relation?
         (d/q '[:find (sum ?heads)
                :in   [[?monster ?heads]]]
              [["Cerberus" 3]
               ["Medusa" 1]
               ["Cyclops" 1]
               ["Chimera" 1]])
         [[4]])))

  (testing "Multiple aggregates, correct grouping with :with"
    (is (tdc/query-relation?
         (d/q '[:find (sum ?heads) (min ?heads) (max ?heads)
                (count ?heads) (count-distinct ?heads)
                :with ?monster
                :in   [[?monster ?heads]]]
              [["Cerberus" 3]
               ["Medusa" 1]
               ["Cyclops" 1]
               ["Chimera" 1]])
         [[6 1 3 4 2]])))
    
    (testing "Min and max are using comparator instead of default compare"
      ;; Wrong: using js '<' operator
      ;; (apply min [:a/b :a-/b :a/c]) => :a-/b
      ;; (apply max [:a/b :a-/b :a/c]) => :a/c
      ;; Correct: use IComparable interface
      ;; (sort compare [:a/b :a-/b :a/c]) => (:a/b :a/c :a-/b)
      (is (tdc/query-relation?
           (d/q '[:find (min ?x) (max ?x)
                  :in [?x ...]]
                [:a-/b :a/b])
           [[:a/b :a-/b]]))

      (is (tdc/query-relation?
           (d/q '[:find (min 2 ?x) (max 2 ?x)
                  :in [?x ...]]
                [:a/b :a-/b :a/c])
           [[[:a/b :a/c] [:a/c :a-/b]]])))

    (testing "Grouping and parameter passing"
      (is (tdc/query-relation?
           (d/q '[:find ?color (max ?amount ?x) (min ?amount ?x)
                  :in   [[?color ?x]] ?amount]
                [[:red 1]  [:red 2] [:red 3] [:red 4] [:red 5]
                 [:blue 7] [:blue 8]]
                3)
           [[:red  [3 4 5] [1 2 3]]
            [:blue [7 8]   [7 8]]])))

    (testing "avg aggregate"
      (is (tdc/query-relation?
           (d/q '[:find (avg ?x)
                  :in [?x ...]]
                [10 15 20 35 75])
           [[31]])))

    (testing "median aggregate"
      (is (tdc/query-relation?
           (d/q '[:find (median ?x)
                  :in [?x ...]]
                [10 15 20 35 75])
           [[20]])))
    
    (testing "variance aggregate"
      (is (tdc/query-relation?
           (d/q '[:find (variance ?x)
                  :in [?x ...]]
                [10 15 20 35 75])
           [[554]])))

    (testing "stddev aggregate"
      (is (tdc/query-relation?
           (d/q '[:find (stddev ?x)
                  :in [?x ...]]
                [10 15 20 35 75])
           [[23.53720459187964]])))

    (testing "Custom aggregates"
      (is (tdc/query-relation?
           (d/q '[:find ?color (aggregate ?agg ?x)
                  :in   [[?color ?x]] ?agg]
                [[:red 1]  [:red 2] [:red 3] [:red 4] [:red 5]
                 [:blue 7] [:blue 8]]
                sort-reverse)
           [[:red [5 4 3 2 1]] [:blue [8 7]]]))

        #?(:clj
           (is (tdc/query-relation?
                (d/q '[:find ?color
                       (datascript.test.query-aggregates/sort-reverse ?x)
                       :in [[?color ?x]]]
                     [[:red 1]  [:red 2] [:red 3] [:red 4] [:red 5]
                      [:blue 7] [:blue 8]])
                [[:red [5 4 3 2 1]] [:blue [8 7]]])))))
