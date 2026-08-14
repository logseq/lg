(ns clojure.core-test.star
  (:require [clojure.test :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists *
  (deftest test-*
    (is (= 1 (*)))
    (is (= 7 (* 7)))
    (is (= 24 (* 1 2 3 4)))
    (is (= -6 (* -2 3)))
    (is (= 12.5 (* 2.5 5)))
    (is (= 12.5 (* 5 2.5)))
    (is (= 12.5M (* 2.5M 5)))
    (is (= ##-Inf (* ##Inf -1)))
    (is (NaN? (* ##Inf 0)))
    (is (= 1 (apply * [])))
    (is (= 24 (apply * [1 2 3 4])))
    (is (= 24 (apply * 2 [3 4])))))
