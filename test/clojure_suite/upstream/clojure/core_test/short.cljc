(ns clojure.core-test.short
  (:require [clojure.test :as t :refer [are deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists short
  (deftest test-short
    (is (int? (short 0)))

    (are [expected x] (= expected (short x))
      -32768 -32768
      0 0
      32767 32767
      1 1N
      0 0N
      -1 -1N
      1 1.0M
      0 0.0M
      -1 -1.0M
      1.1 1.1
      -1.1 -1.1
      1.9 1.9
      1.1 1.1M
      -1.1 -1.1M)

    (is (= -32768.1 (short -32768.1)))
    (is (= -32769 (short -32769)))
    (is (= 32768 (short 32768)))
    (is (= 32767.1 (short 32767.1)))
    (is (= "0" (short "0")))
    (is (= :0 (short :0)))
    (is (= [0] (short [0])))
    (is (= nil (short nil)))))
