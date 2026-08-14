(ns clojure.core-test.zero-qmark
  (:require [clojure.test :as t :refer [are deftest is]]
            [clojure.core-test.number-range :as r]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists] :as p]))

(when-var-exists zero?
  (deftest test-zero?
    (are [expected x] (= expected (zero? x))
      true  0
      true  0.0
      true  0.0M
      true  0N

      false 0.0000001
      false 1
      false -1
      false r/min-int
      false r/max-int
      false 1.0
      false -1.0
      false r/min-double
      false r/max-double
      false ##Inf
      false ##-Inf
      false ##NaN
      false 1N
      false -1N
      false 1.0M
      false -1.0M

      #?@(:cljs []
          :default
          [true  0/2
           false 1/2
           false -1/2]))

    #?@(:native []
        :lpy [(is (= false (zero? nil)))]
        :cljs [(is (= false (zero? nil)))]
        :default [(is (p/thrown? (zero? nil)))])
    #?@(:native []
        :lpy [(is (= false (zero? false)))]
        :cljs [(is (= false (zero? false)))]
        :default [(is (p/thrown? (zero? false)))])
    #?@(:native []
        :lpy [(is (= false (zero? true)))]
        :cljs [(is (= false (zero? true)))]
        :default [(is (p/thrown? (zero? true)))])))
