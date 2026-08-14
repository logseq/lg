(ns clojure.core-test.persistent-bang
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists persistent!
  (deftest test-persistent!
    (testing "maps"
      (is (= {} (persistent! (transient {}))))
      (is (= {:a 1 :b 2} (persistent! (transient {:a 1 :b 2})))))

    (testing "vectors"
      (is (= [] (persistent! (transient []))))
      (is (= [1 2 3] (persistent! (transient [1 2 3])))))

    (testing "sets"
      (is (= #{} (persistent! (transient #{}))))
      (is (= #{:a :b :c} (persistent! (transient #{:a :b :c})))))))
