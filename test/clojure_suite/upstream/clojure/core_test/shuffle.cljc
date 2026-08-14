(ns clojure.core-test.shuffle
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists shuffle
  (deftest test-shuffle
    (testing "shuffle returns a vector permutation"
      (let [input [1 2 3 4]
            actual (shuffle input)]
        (is (vector? actual))
        (is (= (count input) (count actual)))
        (is (= (sort input) (sort actual)))))

    (testing "seqable collection inputs"
      (let [actual (shuffle #{1 2 3})]
        (is (vector? actual))
        (is (= #{1 2 3} (set actual))))
      (let [actual (shuffle '(1 2 3))]
        (is (vector? actual))
        (is (= [1 2 3] (sort actual))))
      (let [actual (shuffle "abc")]
        (is (vector? actual))
        (is (= 3 (count actual)))))

    (testing "empty inputs"
      (is (= [] (shuffle [])))
      (is (= [] (shuffle nil))))))
