(ns clojure.core-test.star-squote
  (:require [clojure.test :refer [deftest is testing]]))

(deftest test-checked-multiply-integers
  (testing "direct arities"
    (is (= 1 (*')))
    (is (= 7 (*' 7)))
    (is (= 6 (*' 2 3)))
    (is (= 24 (*' 1 2 3 4))))

  (testing "apply and first-class calls"
    (let [checked-multiply *']
      (is (= 1 (apply checked-multiply [])))
      (is (= 24 (apply checked-multiply [1 2 3 4])))
      (is (= 24 (apply checked-multiply 1 [2 3 4]))))))

(deftest test-checked-multiply-floats
  (is (= 12.5 (*' 2.5 5)))
  (is (= 12.5 (*' 5 2.5)))
  (is (= ##Inf (*' ##Inf 2)))
  (is (NaN? (*' ##Inf 0))))

(deftest test-checked-multiply-decimals
  (is (= 12.5M (*' 2.5M 5))))
