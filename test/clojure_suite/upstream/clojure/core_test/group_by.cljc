(ns clojure.core-test.group-by
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists group-by
  (deftest test-group-by
    (testing "group keys and item order"
      (is (= {false [0 2 4 6 8] true [1 3 5 7 9]}
             (group-by odd? (range 10))))
      (is (= {1 ["a" "c"] 2 ["bb"]}
             (group-by count ["a" "bb" "c"]))))

    (testing "identity grouping"
      (is (= {0 [0] 1 [1] 2 [2] 3 [3]}
             (group-by identity (range 4)))))

    (testing "empty inputs"
      (is (= {} (group-by odd? [])))
      (is (= {} (group-by odd? nil))))))
