(ns clojure.core-test.take-while
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists take-while
  (deftest test-take-while
    (is (= (range 5) (take-while #(< % 5) (range 10))))
    (is (= '(0 1 2 3 4) (take-while #(< % 5) (range))))
    (is (= '() (take-while #(< % 5) nil)))
    (is (= '() (take-while #(< % 5) [])))

    (is (= [0 1 2 3 4]
           (into [] (take-while #(< % 5)) (range 10))))
    (is (= [] (into [] (take-while #(< % 5)) nil)))
    (is (= [] (into [] (take-while (constantly false)) (range 10))))))
