(ns clojure.core-test.minus
  (:require [clojure.test :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists -
  (deftest test--
    (is (= -3 (- 3)))
    (is (= 3 (- -3)))
    (is (= 5 (- 10 5)))
    (is (= -45 (- 0 1 2 3 4 5 6 7 8 9)))
    (is (= 2.5 (- 5.0 2.5)))
    (is (= 2.5 (- 5 2.5)))
    (is (= 2.5M (- 5.0M 2.5M)))
    (is (= ##-Inf (- ##-Inf 1)))
    (is (NaN? (- ##Inf ##Inf)))
    (is (= -3 (apply - [3])))
    (is (= 5 (apply - [10 5])))
    (is (= 4 (apply - 10 [1 2 3])))))
