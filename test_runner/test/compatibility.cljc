(ns lg-test.compatibility-test
  (:require
   [clojure.test :refer [are deftest is run-tests testing]]))

(def fixture-events (atom (subvec [:seed] 1)))
(def each-count (atom 0))

(defn once-fixture [run]
  (swap! fixture-events conj :before)
  (run)
  (swap! fixture-events conj :after))

(defn each-fixture [run]
  (swap! each-count inc)
  (run))

(def once-fixture-value
  #?(:clj once-fixture
     :cljs {:before #(swap! fixture-events conj :before)
            :after #(swap! fixture-events conj :after)}))

(def each-fixture-value
  #?(:clj each-fixture
     :cljs {:before #(swap! each-count inc)}))

(clojure.test/use-fixtures :once once-fixture-value)
(clojure.test/use-fixtures :each each-fixture-value)

(deftest arithmetic
  (testing "basic arithmetic"
    (is (= [:before] @fixture-events))
    (is (= 1 @each-count))
    (is (= 4 (+ 2 2)) "addition")
    (are [expected actual]
         (= expected actual)
         2 (+ 1 1)
         6 (* 2 3)))
  (testing :keyword-context
    (is true)))

(deftest exceptions
  (testing "portable thrown assertion"
    (is (= 2 @each-count))
    (is (thrown? Exception
                 (throw (ex-info "boom" {}))))
    (is (thrown-with-msg? ExceptionInfo #"bo+m"
                          (throw (ex-info "boom" {}))))
    (is (thrown-msg? "exact boom"
                     (throw (ex-info "exact boom" {}))))))

(run-tests)
