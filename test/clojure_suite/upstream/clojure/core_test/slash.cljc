(ns clojure.core-test.slash
  (:require [clojure.test :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists] :as p]))

(when-var-exists /
  (deftest test-slash
    (is (= 2 (/ 2 1)))
    (is (= #?(:native 15/2 :melange 7.5) (/ 15 2)))
    (is (= #?(:native 1/2 :melange 0.5) (/ 2)))
    (is (= 50 (/ 100 1 2)))
    (is (= 2.5 (/ 5.0 2)))
    (is (= 2.5 (/ 5 2.0)))
    (is (= 2.5M (/ 5.0M 2)))
    #?(:native (is (p/thrown? (/ 1 0)))
       :melange (is (= ##Inf (/ 1 0))))
    #?(:native (is (p/thrown? (/ -1 0)))
       :melange (is (= ##-Inf (/ -1 0))))
    #?(:native (is (p/thrown? (/ 0 0)))
       :melange (is (NaN? (/ 0 0))))
    (is (= #?(:native 1/2 :melange 0.5) (apply / [2])))
    (is (= #?(:native 15/2 :melange 7.5) (apply / [15 2])))
    (is (= 10 (apply / 100 [2 5])))))
