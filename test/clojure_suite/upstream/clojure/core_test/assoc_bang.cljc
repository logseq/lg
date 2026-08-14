(ns clojure.core-test.assoc-bang
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists assoc!
  (deftest test-assoc!
    (testing "transient maps"
      (is (= {:a 1} (persistent! (assoc! (transient {}) :a 1))))
      (is (= {:a 3 :b 2}
             (persistent! (assoc! (transient {:a 1 :b 2}) :a 3))))
      (is (= {:a 1 :b 3 :c 5}
             (persistent! (assoc! (transient {:a 1}) :b 2 :b 3 :c 5)))))

    (testing "transient vectors"
      (is (= [0] (persistent! (assoc! (transient []) 0 0))))
      (is (= [0 3 2]
             (persistent! (assoc! (transient [0 1 2]) 1 3))))
      (is (= [1 3 5 7]
             (persistent! (assoc! (transient [1 2]) 1 3 2 5 3 7)))))))
