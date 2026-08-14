(ns clojure.core-test.conj-bang
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists conj!
  (deftest test-conj!
    (testing "zero and one argument arities"
      (is (= [] (persistent! (conj!))))
      (is (= [1 2] (persistent! (conj! (transient [1 2]))))))

    (testing "transient vectors"
      (is (= [1] (persistent! (conj! (transient []) 1))))
      (is (= [1 2 3 4]
             (persistent! (conj! (transient [1 2]) 3 4)))))

    (testing "transient maps"
      (is (= {:a 1} (persistent! (conj! (transient {}) [:a 1]))))
      (is (= {:a 1 :b 2}
             (persistent! (conj! (transient {:a 1}) [:b 2])))))

    (testing "transient sets"
      (is (= #{1} (persistent! (conj! (transient #{}) 1))))
      (is (= #{1 2 3 4}
             (persistent! (conj! (transient #{1 2}) 3 4)))))))
