(ns clojure.core-test.float
  (:require [clojure.test :as t :refer [are deftest is]]
            [clojure.core-test.number-range :as r]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists] :as p]))

(when-var-exists float
  (deftest test-float
    ;; LG keeps the ClojureScript identity cast but partitions these assertions
    ;; by static numeric domain instead of erasing one `are` closure into a
    ;; universal number representation.
    (is (= (float 1.0) (float 1)))
    (is (= (float 0.0) (float 0)))
    (is (= (float -1.0) (float -1)))
    (is (= (float 1.0) (float 1N)))
    (is (= (float 0.0) (float 0N)))
    (is (= (float -1.0) (float -1N)))
    (is (= 12/12 (float 12/12)))
    (is (= 0/12 (float 0/12)))
    (is (= -12/12 (float -12/12)))
    (is (= 1.0M (float 1.0M)))
    (is (= 0.0M (float 0.0M)))
    (is (= -1.0M (float -1.0M)))
    (is (= r/min-double (float r/min-double)))
    (is (NaN? (float ##NaN)))

    #?@(:native
        [(is (= r/max-double (float r/max-double)))
         (is (= ##Inf (float ##Inf)))
         (is (= ##-Inf (float ##-Inf)))
         (is (= "0" (float "0")))
         (is (= :0 (float :0)))]

        :cljr
        [(is (p/thrown? (float r/max-double)))
         (is (p/thrown? (float ##Inf)))
         (is (p/thrown? (float ##-Inf)))
         (is (= (float 0.0) (float "0")))
         (is (p/thrown? (float :0)))]

        :lpy
        [(is (= r/max-double (float r/max-double)))
         (is (= ##Inf (float ##Inf)))
         (is (= ##-Inf (float ##-Inf)))
         (is (= 0.0 (float "0")))
         (is (p/thrown? (float :0)))]

        :jank
        [(is (= r/max-double (float r/max-double)))
         (is (= ##Inf (float ##Inf)))
         (is (= ##-Inf (float ##-Inf)))
         (is (p/thrown? (float "0")))
         (is (p/thrown? (float :0)))]

        :cljs
        [(is (= r/max-double (float r/max-double)))
         (is (= ##Inf (float ##Inf)))
         (is (= ##-Inf (float ##-Inf)))
         (is (= "0" (float "0")))
         (is (= :0 (float :0)))]

        :default
        [(is (p/thrown? (float r/max-double)))
         (is (p/thrown? (float ##Inf)))
         (is (p/thrown? (float ##-Inf)))
         (is (p/thrown? (float "0")))
         (is (p/thrown? (float :0)))])

    #?@(:native []
        :cljr
        [(is (instance? System.Single (float 0)))
         (is (instance? System.Single (float 0.0)))
         (is (instance? System.Single (float 0N)))
         (is (instance? System.Single (float 0.0M)))]

        :clj
        [(is (instance? java.lang.Float (float 0)))
         (is (instance? java.lang.Float (float 0.0)))
         (is (instance? java.lang.Float (float 0N)))
         (is (instance? java.lang.Float (float 0.0M)))])))
