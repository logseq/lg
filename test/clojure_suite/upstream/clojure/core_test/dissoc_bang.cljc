(ns clojure.core-test.dissoc-bang
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists dissoc!
  (deftest test-dissoc!
    (testing "removing present and repeated keys"
      (is (= {} (persistent! (dissoc! (transient {:a 1}) :a))))
      (is (= {} (persistent! (dissoc! (transient {:a 1}) :a :a))))
      (is (= {:b 2}
             (persistent! (dissoc! (transient {:a 1 :b 2}) :a)))))

    (testing "removing absent keys is idempotent"
      (is (= {:b 2}
             (persistent! (dissoc! (transient {:a 1 :b 2}) :a :c)))))))
