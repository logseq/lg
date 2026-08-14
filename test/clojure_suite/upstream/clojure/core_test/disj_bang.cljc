(ns clojure.core-test.disj-bang
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists disj!
  (deftest test-disj!
    (testing "removing present and repeated values"
      (is (= #{} (persistent! (disj! (transient #{1}) 1))))
      (is (= #{} (persistent! (disj! (transient #{1}) 1 1 1))))
      (is (= #{3} (persistent! (disj! (transient #{1 2 3}) 1 2)))))

    (testing "removing absent values is idempotent"
      (is (= #{1 2 3}
             (persistent! (disj! (transient #{1 2 3}) 4 5 6)))))

    (testing "keyword sets preserve their static element type"
      (is (= #{:a :b}
             (persistent! (disj! (transient #{:a :b :c}) :c)))))))
