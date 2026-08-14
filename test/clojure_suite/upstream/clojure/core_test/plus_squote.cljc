(ns clojure.core-test.plus-squote
  (:require [clojure.test :refer [deftest is testing]]))

(defn ordinary-name []
  1)

(defn ordinary-name' []
  2)

(deftest test-checked-add-integers
  (testing "direct arities"
    (is (= 0 (+')))
    (is (= 7 (+' 7)))
    (is (= 3 (+' 1 2)))
    (is (= 10 (+' 1 2 3 4))))

  (testing "apply and first-class calls"
    (let [checked-add +']
      (is (= 0 (apply checked-add [])))
      (is (= 10 (apply checked-add [1 2 3 4])))
      (is (= 10 (apply checked-add 1 [2 3 4])))))

  (testing "apostrophe symbols have distinct bindings"
    (is (= 1 (ordinary-name)))
    (is (= 2 (ordinary-name')))))

(deftest test-checked-add-floats
  (is (= 7.5 (+' 2.5 5)))
  (is (= 7.5 (+' 5 2.5)))
  (is (= ##Inf (+' ##Inf 1)))
  (is (NaN? (+' ##Inf ##-Inf))))

(deftest test-checked-add-decimals
  (is (= 7.5M (+' 2.5M 5))))
