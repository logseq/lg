(ns clojure.core-test.plus
  (:require [clojure.test :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists +
  (deftest test-+
    (is (= 0 (+)))
    (is (= 7 (+ 7)))
    (is (= 10 (+ 1 2 3 4)))
    (is (= 0 (+ 1 -1)))
    (is (= 7.5 (+ 2.5 5)))
    (is (= 7.5 (+ 5 2.5)))
    (is (= 7.5M (+ 2.5M 5)))
    (is (= ##Inf (+ ##Inf 1)))
    (is (NaN? (+ ##Inf ##-Inf)))
    (is (= 0 (apply + [])))
    (is (= 10 (apply + [1 2 3 4])))
    (is (= 10 (apply + 1 [2 3 4])))))
