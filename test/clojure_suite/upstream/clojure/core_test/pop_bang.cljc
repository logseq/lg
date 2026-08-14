(ns clojure.core-test.pop-bang
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists pop!
  (deftest test-pop!
    (testing "pop! removes the last transient vector value"
      (is (= [] (persistent! (pop! (transient [1])))))
      (is (= [1 2] (persistent! (pop! (transient [1 2 3])))))
      (is (= [:c :b]
             (persistent! (pop! (transient [:c :b :a]))))))))
